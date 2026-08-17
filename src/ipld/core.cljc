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

(defn- byte-vector [value] (mapv #(bit-and % 0xff) (seq value)))

(defn decode
  "DAG-CBOR bytes → Clojure data; tag 42 becomes a `Link`, any other tag throws.

  DAG-CBOR is a *canonical* codec: for any node there is exactly one valid
  encoding. Decoding alone does not enforce that, and the ways it fails to are
  not exotic -- `18 05` reads as the integer 5, out-of-order map keys read as a
  map, and duplicate map keys read as the last one wins. Bytes the encoder
  would never have produced would then be accepted as the node they merely
  resemble, which is the one thing a content-addressed identity layer cannot
  afford: two byte strings would denote one value while having different CIDs.

  So the round trip is required to be an identity, not a coincidence. The cost
  is one extra encode per decode, paid at the layer whose entire job is
  canonical form."
  [bytes]
  (let [node (<-cbor-data (cbor/decode bytes))]
    (data-model/validate! node)
    (let [canonical (cbor/encode (->cbor-data node))]
      (when-not (= (byte-vector bytes) (byte-vector canonical))
        (throw (ex-info "ipld: bytes are not canonical DAG-CBOR"
                        {:ipld/problem :non-canonical-dag-cbor
                         :given-bytes (count (byte-vector bytes))
                         :canonical-bytes (count (byte-vector canonical))}))))
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

(defn- read-varint
  "Unsigned LEB128 at OFFSET of a byte vector, or nil. Bounded at 5 groups so
   a malformed header cannot spin."
  [bytes offset]
  (loop [i offset shift 0 value 0]
    (when (and (< i (count bytes)) (< shift 35))
      (let [b (bit-and (long (nth bytes i)) 0xff)
            value (+ value (bit-shift-left (bit-and b 0x7f) shift))]
        (if (zero? (bit-and b 0x80))
          {:value value :next (inc i)}
          (recur (inc i) (+ shift 7) value))))))

(defn cid-codec
  "Codec number a CIDv1 string declares, or nil when it cannot be read.

   Reading it is what lets a foreign codec be reported as a foreign codec.
   Recomputing a dag-cbor CID over a raw block's bytes produces a different
   string, so without this the traversal reports a CID mismatch -- which sends
   whoever reads the error looking for a corrupt store, when the store was
   right and the link simply pointed at something this codec cannot decode."
  [cid]
  (try
    (let [bytes (vec (mf/cid->bytes cid))
          version (read-varint bytes 0)]
      (when (= 1 (:value version))
        (:value (read-varint bytes (:next version)))))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn get-verified-block
  "Fetch bytes at `expected-cid` and recompute their CID before returning them.
  Missing blocks return nil; a storage adapter returning different bytes under
  a CID fails closed. This is the only conforming read across a storage trust
  boundary."
  [get-fn expected-cid]
  (when-let [bytes (get-fn expected-cid)]
    ;; Checked before the hash comparison, because otherwise a correct raw
    ;; block reports as a mismatch: the recomputation assumes dag-cbor.
    (let [codec (cid-codec expected-cid)]
      (when (and codec (not= codec mf/codec-dag-cbor))
        (throw (ex-info "ipld: block codec is not dag-cbor"
                        {:type :ipld/unsupported-codec
                         :cid expected-cid
                         :codec codec}))))
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
