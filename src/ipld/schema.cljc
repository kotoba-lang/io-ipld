(ns ipld.schema
  "Small, data-driven IPLD Schema unifier over the universal Node interface.

  It implements the load-bearing Schema concepts needed by this stack:
  kinds, lists, tuples, maps, structs, and inline discriminated unions. Schema
  is a lens: successful unification retains the original Node/ADL value."
  (:require [ipld.data-model :as dm]))

(declare valid?)

(defn- fail [path expected value]
  {:valid? false :path path :expected expected
   :actual (try (dm/kind value)
                (catch #?(:clj Exception :cljs :default) _ :invalid))})

(defn- check-struct [schema value path]
  (if (not= :map (dm/kind value))
    (fail path :map value)
    (let [fields (:fields schema)
          required (set (or (:required schema) (keys fields)))
          allowed (set (keys fields))
          actual (set (map first (dm/entries value)))
          missing (seq (remove actual required))
          unknown (when-not (:open? schema) (seq (remove allowed actual)))]
      (cond
        missing {:valid? false :path path :expected :required-fields :missing (vec missing)}
        unknown {:valid? false :path path :expected :known-fields :unknown (vec unknown)}
        :else (or (some (fn [[field field-schema]]
                          (when (contains? actual field)
                            (let [result (valid? field-schema (dm/lookup value field)
                                                 (conj path field))]
                              (when-not (:valid? result) result))))
                        fields)
                  {:valid? true})))))

(defn valid?
  "Validate `value` against a schema data value. Returns a structured result."
  ([schema value] (valid? schema value []))
  ([schema value path]
   (try
     (case (:type schema)
       :any {:valid? true}
       :kind (if (= (:kind schema) (dm/kind value))
               {:valid? true} (fail path (:kind schema) value))
       :list (if (= :list (dm/kind value))
               (or (some (fn [[i item]]
                           (let [result (valid? (:items schema) item (conj path i))]
                             (when-not (:valid? result) result)))
                         (dm/entries value))
                   {:valid? true})
               (fail path :list value))
       :tuple (if (and (= :list (dm/kind value))
                       (= (count (:items schema)) (dm/length value)))
                (or (some (fn [[i item-schema]]
                            (let [result (valid? item-schema (dm/lookup value i)
                                                 (conj path i))]
                              (when-not (:valid? result) result)))
                          (map-indexed vector (:items schema)))
                    {:valid? true})
                (fail path {:tuple-length (count (:items schema))} value))
       :map (if (= :map (dm/kind value))
              (or (some (fn [[k v]]
                          (let [key-result (valid? (:keys schema) k (conj path k :key))
                                value-result (valid? (:values schema) v (conj path k))]
                            (cond (not (:valid? key-result)) key-result
                                  (not (:valid? value-result)) value-result)))
                        (dm/entries value))
                  {:valid? true})
              (fail path :map value))
       :struct (check-struct schema value path)
       :union (let [discriminator (:discriminator schema)
                    tag (when (= :map (dm/kind value)) (dm/lookup value discriminator))
                    member (get (:members schema) tag)]
                (if member
                  (valid? member value path)
                  {:valid? false :path (conj path discriminator)
                   :expected (vec (keys (:members schema))) :actual tag}))
       {:valid? false :path path :expected :known-schema-type :actual (:type schema)})
     (catch #?(:clj Exception :cljs :default) _
       (fail path (:type schema) value)))))

(defn unify
  "Return a typed lens over the original value, or nil when it does not unify."
  [schema-name schema value]
  (let [result (valid? schema value)]
    (when (:valid? result)
      {:schema/name schema-name :schema schema :value value})))

(defn require-unify
  "Unify or fail with the precise schema mismatch."
  [schema-name schema value]
  (let [result (valid? schema value)]
    (if (:valid? result)
      {:schema/name schema-name :schema schema :value value}
      (throw (ex-info "ipld: schema unification failed"
                      (assoc result :type :ipld/schema-mismatch
                             :schema/name schema-name))))))
