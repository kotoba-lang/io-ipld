(ns ipld.graph
  "Bounded, transport-neutral IPLD graph selection.

  This is the common correctness core for a GraphSync adapter and a trustless
  HTTP/CAR gateway. It deliberately does not define either wire protocol:
  callers receive root-first, CID-verified blocks and can frame them as CAR or
  GraphSync messages without reimplementing traversal or resource limits."
  (:require [ipld.core :as ipld]
            [ipld.link :as link]
            [ipld.selector :as selector]))

(defn- positive-limit [limits k]
  (let [n (get limits k)]
    (when-not (and (integer? n) (pos? n))
      (throw (ex-info "ipld: graph traversal requires positive resource limits"
                      {:type :ipld/invalid-limit :limit k :value n})))
    n))

(defn- byte-length [bytes]
  #?(:clj (alength ^bytes bytes) :cljs (.-length bytes)))

(defn select-blocks
  "Resolve `root-cid` with an IPLD selector and return the matched nodes plus
  every unique block needed to prove the result, in root-first order.

  `get-fn` maps CID string to bytes. Every fetched block is rehashed before it
  is decoded. `limits` must contain positive `:max-blocks`, `:max-bytes`,
  `:max-depth`, and `:max-matches` values; traversal fails closed when one is
  exceeded. The result's `:blocks` can be passed directly to a CAR writer."
  [get-fn root-cid selector-data limits]
  (let [max-blocks (positive-limit limits :max-blocks)
        max-bytes (positive-limit limits :max-bytes)
        max-depth (positive-limit limits :max-depth)
        max-matches (positive-limit limits :max-matches)
        state (atom {:seen #{} :bytes 0 :blocks []})
        fetch! (fn [cid]
                 (if (contains? (:seen @state) cid)
                   (ipld/get-node get-fn cid)
                   (let [bytes (or (ipld/get-verified-block get-fn cid)
                                   (throw (ex-info "ipld: selected block is missing"
                                                   {:type :ipld/missing-block :cid cid})))
                         next-count (inc (count (:blocks @state)))
                         next-bytes (+ (:bytes @state) (byte-length bytes))]
                     (when (> next-count max-blocks)
                       (throw (ex-info "ipld: selected graph exceeds block limit"
                                       {:type :ipld/resource-limit
                                        :limit :max-blocks :maximum max-blocks})))
                     (when (> next-bytes max-bytes)
                       (throw (ex-info "ipld: selected graph exceeds byte limit"
                                       {:type :ipld/resource-limit
                                        :limit :max-bytes :maximum max-bytes})))
                     (swap! state (fn [s] (-> s
                                             (update :seen conj cid)
                                             (assoc :bytes next-bytes)
                                             (update :blocks conj {:cid cid :bytes bytes}))))
                     (ipld/decode bytes))))
        root (fetch! root-cid)
        matches (selector/select-graph
                 root selector-data
                 {:resolve-link (fn [link-value path]
                                  (when (> (count path) max-depth)
                                    (throw (ex-info "ipld: selected graph exceeds depth limit"
                                                    {:type :ipld/resource-limit
                                                     :limit :max-depth
                                                     :maximum max-depth
                                                     :path path})))
                                  (fetch! (link/link-cid link-value)))
                  :max-depth max-depth
                  :max-matches max-matches})]
    {:root root-cid
     :selector selector-data
     :matches matches
     :blocks (:blocks @state)
     :stats {:blocks (count (:blocks @state))
             :bytes (:bytes @state)
             :matches (count matches)}}))

(defn path-selector
  "Compile a Data Model path into a selector. Links between segments are
  transparently resolved by `select-blocks`."
  [path]
  (reduce (fn [next-selector segment]
            (if (integer? segment)
              {:selector :explore-index :index segment :next next-selector}
              {:selector :explore-fields :fields {segment next-selector}}))
          {:selector :matcher}
          (reverse path)))

(defn resolve-path
  "Trustless-pathing core: return the selected value and the root-first proof
  blocks needed to verify it. HTTP status, URL escaping, Range semantics, and
  CAR framing remain responsibilities of the gateway adapter."
  [get-fn root-cid path limits]
  (let [result (select-blocks get-fn root-cid (path-selector path) limits)]
    (assoc result :path (vec path) :value (some-> result :matches first :value))))
