(ns ipld.core
  "Canonical IPLD DAG-CBOR layer for kotoba-lang — the one place that turns
  Clojure data + CID links into IPLD blocks and back.

  Composes `kotoba-lang/multiformats` (CIDv1 sha2-256 assembly) and
  `kotoba-lang/dag-cbor` (canonical definite-length CBOR + generic tags)
  into the DAG-CBOR spec's link discipline:

    - a link is CBOR **tag 42** wrapping a byte string of
      `0x00 (identity multibase prefix) ++ <binary CID>` — the exact wire
      form every generic IPLD/IPFS tool (go-ipld-prime, dag-cbor js,
      `ipfs dag get`) expects, replacing the plain-CID-string convention
      the first prolly-tree/quad-store/commit-dag landing used (their
      READMEs carried this as an explicit honesty note, not a surprise);
    - tag 42 is the ONLY tag allowed in a block (per the DAG-CBOR spec) —
      `decode` throws on any other tag, `encode` throws on a raw
      `cbor/tagged` (construct links with `link`, nothing else);
    - map keys must be strings (DAG-CBOR spec) — enforced at `encode`.

  In application data a link is the explicit `Link` wrapper: construct
  with `(link cid-string)`, read with `link-cid`, test with `link?`.
  Access NEVER goes through deftype fields at call sites (`.-cid`) — nbb
  and other lighter cljs runtimes don't implement direct field access,
  which is exactly how earlier portability bugs stayed invisible.

  Storage stays injected exactly like prolly-tree/commit-dag:
  `(put-node! put! node)` encodes, CIDs, stores, returns the CID string.
  `links` deep-collects every Link's CID in a node — the one generic walk
  hydrate loops and GC need (kotoba-client uses it instead of knowing any
  node schema)."
  (:require [clojure.string :as str]
            [multiformats.core :as mf]
            [cbor.core :as cbor]))

;; ── Link ──────────────────────────────────────────────────────────────────────
(deftype Link [cid]
  #?@(:clj  [Object
             (equals [_ other]
               (and (instance? Link other) (= cid (.-cid ^Link other))))
             (hashCode [_] (hash cid))
             (toString [_] (str "#ipld/link \"" cid "\""))]
      :cljs [IEquiv
             (-equiv [_ other]
               (and (instance? Link other) (= cid (.-cid other))))
             IHash
             (-hash [_] (hash cid))
             Object
             (toString [_] (str "#ipld/link \"" cid "\""))]))

(defn link
  "Wrap a base32 'b'-multibase CIDv1 string as an IPLD link."
  [cid-str]
  (when-not (and (string? cid-str) (str/starts-with? cid-str "b"))
    (throw (ex-info "ipld: link expects a base32 'b' multibase CID string"
                    {:cid cid-str})))
  (Link. cid-str))

(defn link? [x] (instance? Link x))

(defn link-cid
  "The CID string inside a Link."
  [^Link l]
  (.-cid l))

;; ── tag-42 byte form: 0x00 identity-multibase prefix ++ binary CID ───────────
(defn- link->tag-bytes [cid-str]
  (let [body (mf/cid->bytes cid-str)]
    #?(:clj (byte-array (cons (byte 0) (seq body)))
       :cljs (js/Uint8Array. (clj->js (cons 0 (seq body)))))))

(defn- tag-bytes->link [bs]
  (let [xs (map #(bit-and (int %) 0xff) (seq bs))]
    (when-not (= 0 (first xs))
      (throw (ex-info "ipld: tag-42 byte string must start with the 0x00 identity multibase prefix"
                      {:first-byte (first xs)})))
    (Link. (str "b" (mf/base32 (rest xs))))))

;; ── data <-> cbor-with-tags transforms ────────────────────────────────────────
(defn- ->cbor-data [x]
  (cond
    (link? x)        (cbor/tagged 42 (link->tag-bytes (link-cid x)))
    (cbor/tagged? x) (throw (ex-info "ipld: raw cbor tags are not IPLD data — construct links with ipld.core/link"
                                     {:tag (cbor/tag-number x)}))
    (map? x)         (into {}
                           (map (fn [[k v]]
                                  (when-not (or (string? k) (keyword? k))
                                    (throw (ex-info "ipld: DAG-CBOR map keys must be strings"
                                                    {:key k})))
                                  [k (->cbor-data v)]))
                           x)
    (sequential? x)  (mapv ->cbor-data x)
    :else x))

(defn- <-cbor-data [x]
  (cond
    (cbor/tagged? x) (if (= 42 (cbor/tag-number x))
                       (tag-bytes->link (cbor/tag-value x))
                       (throw (ex-info "ipld: DAG-CBOR allows tag 42 only"
                                       {:tag (cbor/tag-number x)})))
    (map? x)         (into {} (map (fn [[k v]] [k (<-cbor-data v)])) x)
    (sequential? x)  (mapv <-cbor-data x)
    :else x))

;; ── public codec surface ──────────────────────────────────────────────────────
(defn encode
  "Canonical DAG-CBOR bytes for `node` (Clojure data; `Link`s become tag 42)."
  [node]
  (cbor/encode (->cbor-data node)))

(defn decode
  "DAG-CBOR bytes → Clojure data; tag 42 becomes a `Link`, any other tag throws."
  [bytes]
  (<-cbor-data (cbor/decode bytes)))

(defn cid
  "CIDv1 dag-cbor sha2-256 of already-encoded block bytes."
  [bytes]
  (mf/cidv1-dag-cbor bytes))

(defn node->block
  "Encode `node` and address it: `{:cid <string> :bytes <bytes>}`."
  [node]
  (let [bytes (encode node)]
    {:cid (cid bytes) :bytes bytes}))

(defn put-node!
  "Encode `node`, CID it, `(put! cid bytes)`, return the CID string — the
  same storage-port convention prolly-tree/quad-store/commit-dag use."
  [put! node]
  (let [{:keys [cid bytes]} (node->block node)]
    (put! cid bytes)
    cid))

(defn get-node
  "Fetch and decode the node at `cid-str` via `(get-fn cid) -> bytes`.
  Returns nil when `get-fn` does (block not present)."
  [get-fn cid-str]
  (when-let [bytes (get-fn cid-str)]
    (decode bytes)))

(defn links
  "Every Link CID reachable inside `node` (deep, document order). This is
  the generic block-graph walk: hydrate loops and GC traverse a DAG by
  `links` alone, with zero knowledge of any node schema."
  [node]
  (cond
    (link? node)       [(link-cid node)]
    (map? node)        (vec (mapcat links (vals node)))
    (sequential? node) (vec (mapcat links node))
    :else              []))
