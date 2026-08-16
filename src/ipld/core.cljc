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
    - map keys must be strings (DAG-CBOR spec) — enforced at `encode` and
      after `decode`; host keywords are application values, not IPLD keys.

  In application data a link is the explicit `Link` wrapper: construct
  with `(link cid-string)`, read with `link-cid`, test with `link?`.
  Access NEVER goes through representation fields at call sites — use the
  narrow accessor so JVM, compiled ClojureScript, and SCI/nbb agree.

  Storage stays injected exactly like prolly-tree/commit-dag:
  `(put-node! put! node)` encodes, CIDs, stores, returns the CID string.
  `links` deep-collects every Link's CID in a node — the one generic walk
  hydrate loops and GC need (kotoba-client uses it instead of knowing any
  node schema)."
  (:require [multiformats.core :as mf]
            [cbor.core :as cbor]
            [ipld.link :as link-value]
            [ipld.data-model :as data-model]))

;; Compatibility aliases keep the existing `ipld.core` API while allowing
;; value-only consumers to load `ipld.link` without the hashing namespace.
(def link link-value/link)
(def link? link-value/link?)
(def link-cid link-value/link-cid)
(def link->tag link-value/link->tag)
(def tag->link link-value/tag->link)

;; ── data <-> cbor-with-tags transforms ────────────────────────────────────────
(defn- ->cbor-data [x]
  (cond
    (link? x)        (link->tag x)
    (cbor/tagged? x) (throw (ex-info "ipld: raw cbor tags are not IPLD data — construct links with ipld.core/link"
                                     {:tag (cbor/tag-number x)}))
    (map? x)         (into {}
                           (map (fn [[k v]]
                                  (when-not (string? k)
                                    (throw (ex-info "ipld: DAG-CBOR map keys must be strings"
                                                    {:key k})))
                                  [k (->cbor-data v)]))
                           x)
    (sequential? x)  (mapv ->cbor-data x)
    :else x))

(defn- <-cbor-data [x]
  (cond
    (cbor/tagged? x) (if (= 42 (cbor/tag-number x))
                       (tag->link x)
                       (throw (ex-info "ipld: DAG-CBOR allows tag 42 only"
                                       {:tag (cbor/tag-number x)})))
    (map? x)         (into {} (map (fn [[k v]] [k (<-cbor-data v)])) x)
    (sequential? x)  (mapv <-cbor-data x)
    :else x))

;; ── public codec surface ──────────────────────────────────────────────────────
(defn encode
  "Canonical DAG-CBOR bytes for `node` (Clojure data; `Link`s become tag 42)."
  [node]
  (data-model/validate! node)
  (cbor/encode (->cbor-data node)))

(defn decode
  "DAG-CBOR bytes → Clojure data; tag 42 becomes a `Link`, any other tag throws."
  [bytes]
  (let [node (<-cbor-data (cbor/decode bytes))]
    (data-model/validate! node)
    node))

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

(defn get-verified-block
  "Fetch bytes at `expected-cid` and recompute their CID before returning them.
  Missing blocks return nil; a storage adapter returning different bytes under
  a CID fails closed. This is the only conforming read across a storage trust
  boundary."
  [get-fn expected-cid]
  (when-let [bytes (get-fn expected-cid)]
    (let [actual (cid bytes)]
      (when-not (= expected-cid actual)
        (throw (ex-info "ipld: block CID mismatch"
                        {:type :ipld/cid-mismatch
                         :expected-cid expected-cid
                         :actual-cid actual})))
      bytes)))

(defn get-node
  "Fetch and decode the node at `cid-str` via `(get-fn cid) -> bytes`.
  Returns nil when `get-fn` does (block not present). Bytes are rehashed before
  decode; callers cannot accidentally treat a CID-keyed lookup as verification."
  [get-fn cid-str]
  (when-let [bytes (get-verified-block get-fn cid-str)]
    (decode bytes)))

#?(:cljs
   (defn get-node-async
     "Promise variant for async object stores; applies the identical CID
     recomputation before decode."
     [get-fn expected-cid]
     (-> (get-fn expected-cid)
         (.then (fn [bytes]
                  (when bytes
                    (let [actual (cid bytes)]
                      (when-not (= expected-cid actual)
                        (throw (ex-info "ipld: block CID mismatch"
                                        {:type :ipld/cid-mismatch
                                         :expected-cid expected-cid
                                         :actual-cid actual})))
                      (decode bytes))))))))

(defn links
  "Every Link CID reachable inside `node` (deep, document order). This is
  the generic block-graph walk: hydrate loops and GC traverse a DAG by
  `links` alone, with zero knowledge of any node schema."
  [node]
  (cond
    (link? node)       [(link-cid node)]
    (map? node)        (vec (mapcat (comp links val) (sort-by key node)))
    (sequential? node) (vec (mapcat links node))
    :else              []))
