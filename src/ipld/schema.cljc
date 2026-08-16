(ns ipld.schema
  "Bounded IPLD Schema DMT validation and representation unification.

  Schema DMT is the authority. `compile-schema` validates its shape and all
  named references; `valid?`/`unify!` then match plain IPLD Data Model values
  without changing their content identity."
  (:require [clojure.string :as str]
            [ipld.data-model :as dm]
            [ipld.link :as link]))

(def prelude
  {"Any" {"any" {}} "Bool" {"bool" {}} "String" {"string" {}}
   "Bytes" {"bytes" {}} "Int" {"int" {}} "Float" {"float" {}}
   "Map" {"map" {"keyType" "String" "valueType" "Any"}}
   "List" {"list" {"valueType" "Any"}} "Link" {"link" {}}})

(defn- fail! [problem data]
  (throw (ex-info (str "ipld schema: " (name problem))
                  (assoc data :type :ipld/schema :problem problem))))

(defn- bytes-like? [x]
  #?(:clj (bytes? x)
     :cljs (or (instance? js/Uint8Array x) (instance? js/Int8Array x))))

(defn- data-kind [value]
  (cond (nil? value) "null" (boolean? value) "bool"
        (integer? value) "int" (number? value) "float"
        (string? value) "string" (bytes-like? value) "bytes"
        (link/link? value) "link" (map? value) "map"
        (sequential? value) "list" :else nil))

(defn- byte-vector [value] (mapv #(bit-and % 0xff) (seq value)))
(defn- bytes-from [octets]
  #?(:clj (byte-array (map unchecked-byte octets))
     :cljs (js/Uint8Array.from (clj->js octets))))
(defn- hex-bytes [text]
  (when-not (and (string? text) (pos? (count text))
                 (even? (count text)) (re-matches #"[0-9A-F]+" text))
    (fail! :invalid-bytes-prefix {:prefix text}))
  (mapv (fn [i]
          #?(:clj (Integer/parseInt (subs text i (+ i 2)) 16)
             :cljs (js/parseInt (subs text i (+ i 2)) 16)))
        (range 0 (count text) 2)))
(defn- starts-with-bytes? [value prefix]
  (= prefix (vec (take (count prefix) value))))

(defn- one-entry! [definition where]
  (when-not (and (map? definition) (= 1 (count definition)))
    (fail! :definition-must-have-one-kind {:where where :definition definition}))
  (first definition))

(defn- type-refs [definition]
  (let [[kind body] (one-entry! definition :reference-scan)]
    (case kind
      "copy" [(get body "fromType")]
      "link" (cond-> [] (get body "expectedType") (conj (get body "expectedType")))
      "list" (let [t (get body "valueType")] (if (string? t) [t] (type-refs t)))
      "map" (let [k (get body "keyType") v (get body "valueType")]
                (into [k] (if (string? v) [v] (type-refs v))))
      "struct" (mapcat (fn [[_ field]]
                          (let [t (get field "type")]
                            (if (string? t) [t] (type-refs t))))
                        (get body "fields"))
      "union" (mapcat #(if (string? %) [%] (type-refs %)) (get body "members"))
      [])))

(defn- adl-refs [definition]
  (let [[kind body] (one-entry! definition :adl-scan)
        own (let [representation (get body "representation")]
              (cond
                (string? (get representation "advanced"))
                [(get representation "advanced")]
                :else []))]
    (into own
          (case kind
            "list" (let [t (get body "valueType")] (if (map? t) (adl-refs t) []))
            "map" (let [t (get body "valueType")] (if (map? t) (adl-refs t) []))
            "struct" (mapcat (fn [[_ field]]
                               (let [t (get field "type")]
                                 (if (map? t) (adl-refs t) [])))
                             (get body "fields"))
            "union" (mapcat #(if (map? %) (adl-refs %) []) (get body "members"))
            []))))

(def supported-kinds
  #{"bool" "string" "bytes" "int" "float" "any" "map" "list"
    "link" "struct" "union" "enum" "unit" "copy"})

(defn compile-schema
  "Validate normalized DMT and return an immutable compiled schema map."
  [dmt]
  (when-not (= #{"types"} (set (remove #{"advanced"} (keys dmt))))
    (fail! :invalid-schema-root {:keys (set (keys dmt))}))
  (let [types (get dmt "types")
        advanced (get dmt "advanced" {})]
    (when-not (and (map? types) (every? string? (keys types)))
      (fail! :types-map-required {}))
    (when-not (and (map? advanced) (every? string? (keys advanced)))
      (fail! :advanced-map-required {}))
    (doseq [[name definition] types]
      (when-not (re-matches #"[A-Z][A-Za-z0-9_]*" name)
        (fail! :invalid-type-name {:name name}))
      (let [[kind _] (one-entry! definition name)]
        (when-not (contains? supported-kinds kind)
          (fail! :unsupported-type-kind {:name name :kind kind}))))
    (let [all-types (merge prelude types)]
      (doseq [[name definition] types
              reference (type-refs definition)]
        (when-not (contains? all-types reference)
          (fail! :unknown-type-reference {:type-name name :reference reference})))
      (doseq [[name definition] types
              adl (adl-refs definition)]
        (when-not (contains? advanced adl)
          (fail! :unknown-advanced-reference {:type-name name :advanced adl})))
      {:dmt dmt :types all-types :advanced (set (keys advanced))})))

(defn- positive-limit! [limits key]
  (let [n (get limits key)]
    (when-not (and (integer? n) (pos? n))
      (fail! :positive-limit-required {:limit key :value n})) n))

(defn- consume! [state depth path]
  (when (> depth (:max-depth state))
    (fail! :max-depth-exceeded {:path path :max-depth (:max-depth state)}))
  (let [n (swap! (:nodes state) inc)]
    (when (> n (:max-nodes state))
      (fail! :max-nodes-exceeded {:path path :max-nodes (:max-nodes state)}))))

(declare unify-ref!)

(defn- nullable-ref! [compiled state ref nullable? value depth path]
  (if (nil? value)
    (when-not nullable? (fail! :unexpected-null {:path path}))
    (unify-ref! compiled state ref value depth path)))

(defn- map-representation! [compiled state body value depth path]
  (let [representation (get body "representation")]
    (cond
      (or (nil? representation) (contains? representation "map"))
      (do
        (when-not (and (map? value) (every? string? (keys value)))
          (fail! :expected-map {:path path :actual (data-kind value)}))
        (doseq [[k v] value]
          (unify-ref! compiled state (get body "keyType") k (inc depth) (conj path k :key))
          (nullable-ref! compiled state (get body "valueType")
                         (true? (get body "valueNullable")) v
                         (inc depth) (conj path k))))

      (contains? representation "listpairs")
      (do
        (when-not (sequential? value) (fail! :expected-listpairs {:path path}))
        (doseq [[i pair] (map-indexed vector value)]
          (when-not (and (sequential? pair) (= 2 (count pair)))
            (fail! :invalid-listpair {:path (conj path i)}))
          (unify-ref! compiled state (get body "keyType") (first pair)
                      (inc depth) (conj path i 0))
          (nullable-ref! compiled state (get body "valueType")
                         (true? (get body "valueNullable")) (second pair)
                         (inc depth) (conj path i 1))))

      (contains? representation "advanced")
      (let [name (get representation "advanced")
            validator (get-in state [:adl-validators name])]
        (when-not validator (fail! :missing-adl-validator {:adl name :path path}))
        (when-not (validator value) (fail! :adl-rejected {:adl name :path path})))

      :else (fail! :unsupported-map-representation {:representation representation}))))

(defn- struct-map! [compiled state body details value depth path]
  (when-not (and (map? value) (every? string? (keys value)))
    (fail! :expected-struct-map {:path path :actual (data-kind value)}))
  (let [fields (get body "fields")
        field-details (get details "fields" {})
        wire->field (into {} (map (fn [[field _]]
                                    [(get-in field-details [field "rename"] field) field]) fields))]
    (doseq [wire-key (keys value)]
      (when-not (contains? wire->field wire-key)
        (fail! :unknown-struct-field {:path (conj path wire-key)})))
    (doseq [[field spec] fields]
      (let [wire-key (get-in field-details [field "rename"] field)
            present? (contains? value wire-key)
            implicit? (contains? (get field-details field {}) "implicit")]
        (when (and (not present?) (not (true? (get spec "optional"))) (not implicit?))
          (fail! :missing-struct-field {:path (conj path wire-key)}))
        (when present?
          (nullable-ref! compiled state (get spec "type")
                         (true? (get spec "nullable")) (get value wire-key)
                         (inc depth) (conj path wire-key)))))))

(defn- struct-representation! [compiled state body value depth path]
  (let [representation (get body "representation" {"map" {}})]
    (cond
      (contains? representation "map")
      (struct-map! compiled state body (get representation "map") value depth path)

      (contains? representation "tuple")
      (let [fields (vec (get body "fields"))]
        (when-not (and (sequential? value) (= (count fields) (count value)))
          (fail! :invalid-struct-tuple {:path path :expected (count fields)}))
        (doseq [[[field spec] item] (map vector fields value)]
          (nullable-ref! compiled state (get spec "type")
                         (true? (get spec "nullable")) item
                         (inc depth) (conj path field))))

      :else (fail! :unsupported-struct-representation {:representation representation}))))

(defn- union-representation! [compiled state body value depth path]
  (let [representation (get body "representation")]
    (cond
      (contains? representation "keyed")
      (do
        (when-not (and (map? value) (= 1 (count value)))
          (fail! :invalid-keyed-union {:path path}))
        (let [[discriminant content] (first value)
              member (get-in representation ["keyed" discriminant])]
          (when-not member (fail! :unknown-union-discriminant {:path path :value discriminant}))
          (unify-ref! compiled state member content (inc depth) (conj path discriminant))))

      (contains? representation "kinded")
      (let [kind (data-kind value)
            member (get-in representation ["kinded" kind])]
        (when-not member (fail! :unknown-union-kind {:path path :kind kind}))
        (unify-ref! compiled state member value (inc depth) path))

      (contains? representation "envelope")
      (let [{disc-key "discriminantKey" content-key "contentKey" table "discriminantTable"}
            (get representation "envelope")]
        (when-not (and (map? value) (= #{disc-key content-key} (set (keys value))))
          (fail! :invalid-envelope-union {:path path}))
        (let [member (get table (get value disc-key))]
          (when-not member (fail! :unknown-union-discriminant {:path path}))
          (unify-ref! compiled state member (get value content-key) (inc depth)
                      (conj path content-key))))

      (contains? representation "inline")
      (let [{disc-key "discriminantKey" table "discriminantTable"}
            (get representation "inline")]
        (when-not (and (map? value) (string? (get value disc-key)))
          (fail! :invalid-inline-union {:path path}))
        (let [discriminant (get value disc-key)
              member (get table discriminant)]
          (when-not member (fail! :unknown-union-discriminant
                                  {:path path :value discriminant}))
          (unify-ref! compiled state member (dissoc value disc-key)
                      (inc depth) path)))

      (contains? representation "stringprefix")
      (do
        (when-not (string? value) (fail! :expected-string-prefix-union {:path path}))
        (let [matches (filter (fn [[prefix _]] (str/starts-with? value prefix))
                              (get-in representation ["stringprefix" "prefixes"]))]
          (when-not (= 1 (count matches))
            (fail! :ambiguous-or-missing-union-prefix {:path path :matches (count matches)}))
          (let [[prefix member] (first matches)]
            (unify-ref! compiled state member (subs value (count prefix))
                        (inc depth) path))))

      (contains? representation "bytesprefix")
      (do
        (when-not (bytes-like? value) (fail! :expected-bytes-prefix-union {:path path}))
        (let [octets (byte-vector value)
              matches (keep (fn [[prefix member]]
                              (let [prefix-bytes (hex-bytes prefix)]
                                (when (starts-with-bytes? octets prefix-bytes)
                                  [prefix-bytes member])))
                            (get-in representation ["bytesprefix" "prefixes"]))]
          (when-not (= 1 (count matches))
            (fail! :ambiguous-or-missing-union-prefix {:path path :matches (count matches)}))
          (let [[prefix member] (first matches)]
            (unify-ref! compiled state member
                        (bytes-from (drop (count prefix) octets))
                        (inc depth) path))))

      :else (fail! :unsupported-union-representation {:representation representation}))))

(defn- enum-representation! [body value path]
  (let [members (set (get body "members"))
        representation (get body "representation" {"string" {}})]
    (cond
      (contains? representation "string")
      (let [mapping (get representation "string")
            allowed (set (map #(get mapping % %) members))]
        (when-not (contains? allowed value) (fail! :invalid-enum-value {:path path :value value})))
      (contains? representation "int")
      (when-not (contains? (set (vals (get representation "int"))) value)
        (fail! :invalid-enum-value {:path path :value value}))
      :else (fail! :unsupported-enum-representation {:representation representation}))))

(defn- unify-definition! [compiled state definition value depth path]
  (consume! state depth path)
  (let [[kind body] (one-entry! definition path)]
    (case kind
      "any" true
      "bool" (when-not (boolean? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "string" (when-not (string? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "bytes" (if-let [advanced (get-in body ["representation" "advanced"])]
                  (let [validator (get-in state [:adl-validators advanced])]
                    (when-not validator (fail! :missing-adl-validator {:adl advanced :path path}))
                    (when-not (validator value) (fail! :adl-rejected {:adl advanced :path path})))
                  (when-not (bytes-like? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)})))
      "int" (when-not (integer? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "float" (when-not (and (number? value) (not (integer? value)))
                  (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "link" (when-not (link/link? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "copy" (unify-ref! compiled state (get body "fromType") value (inc depth) path)
      "list" (do (when-not (sequential? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
                   (doseq [[i item] (map-indexed vector value)]
                     (nullable-ref! compiled state (get body "valueType")
                                    (true? (get body "valueNullable")) item
                                    (inc depth) (conj path i))))
      "map" (map-representation! compiled state body value depth path)
      "struct" (struct-representation! compiled state body value depth path)
      "union" (union-representation! compiled state body value depth path)
      "enum" (enum-representation! body value path)
      "unit" (let [expected (case (get body "representation")
                                    "null" nil "true" true "false" false "emptymap" {})]
                 (when-not (= expected value) (fail! :invalid-unit-value {:path path :value value})))
      (fail! :unsupported-type-kind {:kind kind :path path})))
  true)

(defn- unify-ref! [compiled state ref value depth path]
  (if (string? ref)
    (let [definition (get-in compiled [:types ref])]
      (when-not definition (fail! :unknown-type-reference {:reference ref :path path}))
      (unify-definition! compiled state definition value depth path))
    (unify-definition! compiled state ref value depth path)))

(defn unify!
  "Unify VALUE with named TYPE under mandatory `:max-depth`/`:max-nodes`.
  Optional `:adl-validators` maps ADL names to predicate capabilities."
  [compiled type-name value limits]
  (let [state {:max-depth (positive-limit! limits :max-depth)
               :max-nodes (positive-limit! limits :max-nodes)
               :nodes (atom 0)
               :adl-validators (or (:adl-validators limits) {})}]
    (unify-ref! compiled state type-name value 0 [])
    {:type type-name :nodes @(:nodes state) :value value}))

(declare legacy-valid?)

(defn- legacy-fail [path expected value]
  {:valid? false :path path :expected expected
   :actual (try (dm/kind value)
                (catch #?(:clj Exception :cljs :default) _ :invalid))})

(defn- legacy-struct [schema value path]
  (if (not= :map (dm/kind value))
    (legacy-fail path :map value)
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
                            (let [result (legacy-valid? field-schema
                                                        (dm/lookup value field)
                                                        (conj path field))]
                              (when-not (:valid? result) result))))
                        fields)
                  {:valid? true})))))

(defn- legacy-valid? [schema value path]
  (try
    (case (:type schema)
      :any {:valid? true}
      :kind (if (= (:kind schema) (dm/kind value))
              {:valid? true} (legacy-fail path (:kind schema) value))
      :list (if (= :list (dm/kind value))
              (or (some (fn [[i item]]
                          (let [result (legacy-valid? (:items schema) item (conj path i))]
                            (when-not (:valid? result) result)))
                        (dm/entries value))
                  {:valid? true})
              (legacy-fail path :list value))
      :tuple (if (and (= :list (dm/kind value))
                      (= (count (:items schema)) (dm/length value)))
               (or (some (fn [[i item-schema]]
                           (let [result (legacy-valid? item-schema (dm/lookup value i)
                                                       (conj path i))]
                             (when-not (:valid? result) result)))
                         (map-indexed vector (:items schema)))
                   {:valid? true})
               (legacy-fail path {:tuple-length (count (:items schema))} value))
      :map (if (= :map (dm/kind value))
             (or (some (fn [[k v]]
                         (let [kr (legacy-valid? (:keys schema) k (conj path k :key))
                               vr (legacy-valid? (:values schema) v (conj path k))]
                           (cond (not (:valid? kr)) kr (not (:valid? vr)) vr)))
                       (dm/entries value))
                 {:valid? true})
             (legacy-fail path :map value))
      :struct (legacy-struct schema value path)
      :union (let [discriminator (:discriminator schema)
                   tag (when (= :map (dm/kind value)) (dm/lookup value discriminator))
                   member (get (:members schema) tag)]
               (if member (legacy-valid? member value path)
                   {:valid? false :path (conj path discriminator)
                    :expected (vec (keys (:members schema))) :actual tag}))
      {:valid? false :path path :expected :known-schema-type :actual (:type schema)})
    (catch #?(:clj Exception :cljs :default) _
      (legacy-fail path (:type schema) value))))

(defn valid?
  "Validate either the legacy schema algebra (2/3 args, structured result) or
  a compiled Schema DMT (4 args, boolean)."
  ([legacy-schema value] (legacy-valid? legacy-schema value []))
  ([legacy-schema value path] (legacy-valid? legacy-schema value path))
  ([compiled type-name value limits]
   (try (unify! compiled type-name value limits) true
        (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) _ false))))

(defn unify
  "Backward-compatible typed lens for the original data-driven algebra."
  [schema-name legacy-schema value]
  (let [result (valid? legacy-schema value)]
    (when (:valid? result)
      {:schema/name schema-name :schema legacy-schema :value value})))

(defn require-unify
  "Backward-compatible throwing unifier for the original algebra."
  [schema-name legacy-schema value]
  (let [result (valid? legacy-schema value)]
    (if (:valid? result)
      {:schema/name schema-name :schema legacy-schema :value value}
      (throw (ex-info "ipld: schema unification failed"
                      (assoc result :type :ipld/schema-mismatch
                             :schema/name schema-name))))))
