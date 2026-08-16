(ns ipld.selector
  "Minimal IPLD selector subset over native, schema, or ADL Nodes.

  The executable representation uses descriptive keywords. `to-data-model`
  and `from-data-model` are the strict boundary to the compact Selector IPLD
  Schema representation; `encode`/`decode` frame that value as DAG-CBOR."
  (:require [ipld.data-model :as dm]
            [ipld.core :as ipld]
            [ipld.link :as link]))

(defn- invalid-selector [message data]
  (throw (ex-info message (assoc data :type :ipld/invalid-selector))))

(defn- exact-keys! [value expected context]
  (when-not (and (map? value) (= expected (set (keys value))))
    (invalid-selector "ipld: malformed selector data model"
                      {:context context :expected-keys expected :value value})))

(declare to-data-model from-data-model)

(defn to-data-model
  "Compile the supported executable selector subset to the compact IPLD
  Selector Data Model representation from the prescriptive draft schema.

  Supported forms are Matcher, ExploreAll, and ExploreFields. Unsupported
  conditions, labels, recursion, ranges, indexes, and unions fail closed."
  [selector]
  (case (:selector selector)
    :matcher
    (do (exact-keys! selector #{:selector} :matcher)
        {"." {}})

    :explore-all
    (do (exact-keys! selector #{:selector :next} :explore-all)
        {"a" {">" (to-data-model (:next selector))}})

    :explore-fields
    (do
      (exact-keys! selector #{:selector :fields} :explore-fields)
      (when-not (and (map? (:fields selector))
                     (every? string? (keys (:fields selector))))
        (invalid-selector "ipld: ExploreFields keys must be strings"
                          {:selector selector}))
      {"f" {"f>" (into {} (map (fn [[field next-selector]]
                                  [field (to-data-model next-selector)]))
                              (:fields selector))}})

    (invalid-selector "ipld: unsupported selector"
                      {:selector selector :supported #{:matcher :explore-all :explore-fields}})))

(defn from-data-model
  "Parse the supported compact IPLD Selector Data Model representation.
  Shape checks are exact so an unsupported selector is never silently widened
  or interpreted as a nearby supported form."
  [value]
  (do
    (when-not (and (map? value) (= 1 (count value)))
      (invalid-selector "ipld: selector must be a single-member keyed union"
                        {:value value}))
    (let [[tag body] (first value)]
      (case tag
        "." (do (exact-keys! body #{} :matcher)
                {:selector :matcher})
        "a" (do (exact-keys! body #{">"} :explore-all)
                {:selector :explore-all
                 :next (from-data-model (get body ">"))})
        "f" (do
              (exact-keys! body #{"f>"} :explore-fields)
              (let [fields (get body "f>")]
                (when-not (and (map? fields) (every? string? (keys fields)))
                  (invalid-selector "ipld: ExploreFields payload must be a string-keyed map"
                                    {:value fields}))
                {:selector :explore-fields
                 :fields (into {} (map (fn [[field next-value]]
                                         [field (from-data-model next-value)]))
                               fields)}))
        (invalid-selector "ipld: unsupported selector union member"
                          {:tag tag :supported #{"." "a" "f"}})))))

(defn encode
  "Encode a supported selector as canonical DAG-CBOR bytes."
  [selector]
  (ipld/encode (to-data-model selector)))

(defn decode
  "Decode canonical DAG-CBOR selector bytes into the executable subset."
  [bytes]
  (from-data-model (ipld/decode bytes)))

(declare select*)

(defn- branch? [node]
  (contains? #{:map :list} (dm/kind node)))

(defn- select* [node selector path]
  (case (:selector selector)
    :matcher [{:path path :value node}]
    :explore-fields
    (mapcat (fn [[segment next-selector]]
              (when (dm/contains-segment? node segment)
                (let [child (dm/lookup node segment)]
                  (select* child next-selector (conj path segment)))))
            (:fields selector))
    :explore-all
    (mapcat (fn [[segment child]]
              (select* child (:next selector) (conj path segment)))
            (when (branch? node) (dm/entries node)))
    (throw (ex-info "ipld: unknown selector" {:selector selector :path path}))))

(defn select
  "Return `{:path ... :value ...}` matches described by selector data."
  [node selector]
  (vec (select* node selector [])))

(declare select-graph*)

(defn- dereference [node path resolve-link]
  (loop [value node]
    (if (link/link? value)
      (recur (resolve-link value path))
      value)))

(defn- select-graph* [node selector path {:keys [resolve-link max-depth]}]
  (when (> (count path) max-depth)
    (throw (ex-info "ipld: selector exceeds depth limit"
                    {:type :ipld/resource-limit :limit :max-depth
                     :maximum max-depth :path path})))
  (let [node (dereference node path resolve-link)]
    (case (:selector selector)
      :matcher [{:path path :value node}]
      :explore-fields
      (mapcat (fn [[segment next-selector]]
                (when (dm/contains-segment? node segment)
                  (let [child (dm/lookup node segment)]
                    (select-graph* child next-selector (conj path segment)
                                   {:resolve-link resolve-link :max-depth max-depth}))))
              (sort-by (comp pr-str key) (:fields selector)))
      :explore-all
      (mapcat (fn [[segment child]]
                (select-graph* child (:next selector) (conj path segment)
                               {:resolve-link resolve-link :max-depth max-depth}))
              (when (branch? node) (dm/entries node)))
      (throw (ex-info "ipld: unknown selector"
                      {:type :ipld/invalid-selector :selector selector :path path})))))

(defn select-graph
  "Traverse through Links using `:resolve-link`. `:max-depth` and
  `:max-matches` are mandatory resource bounds. Paths remain logical Data
  Model paths: dereferencing a Link does not add an artificial segment."
  [node selector {:keys [resolve-link max-depth max-matches] :as options}]
  (when-not (fn? resolve-link)
    (throw (ex-info "ipld: graph selector requires :resolve-link"
                    {:type :ipld/invalid-selector-options})))
  (when-not (and (integer? max-depth) (pos? max-depth)
                 (integer? max-matches) (pos? max-matches))
    (throw (ex-info "ipld: graph selector requires positive limits"
                    {:type :ipld/invalid-selector-options :options options})))
  (let [matches (vec (select-graph* node selector [] options))]
    (when (> (count matches) max-matches)
      (throw (ex-info "ipld: selector exceeds match limit"
                      {:type :ipld/resource-limit :limit :max-matches
                       :maximum max-matches})))
    matches))
