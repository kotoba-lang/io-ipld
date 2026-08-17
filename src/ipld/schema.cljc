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

(defn- struct-field-order! [body details representation]
  (let [declared (vec (keys (get body "fields")))
        requested (get details "fieldOrder")
        order (if (nil? requested) declared (vec requested))]
    (when (and requested
               (or (not (sequential? requested))
                   (not (every? string? requested))
                   (not= (count order) (count (distinct order)))
                   (not= (set order) (set declared))))
      (fail! :invalid-field-order
             {:representation representation
              :declared declared :field-order requested}))
    order))

(defn- delimiters! [details representation]
  (let [inner (get details "innerDelim")
        entry (get details "entryDelim")]
    (when-not (and (string? inner) (pos? (count inner))
                   (string? entry) (pos? (count entry))
                   (not= inner entry))
      (fail! :invalid-stringpairs-delimiters
             {:representation representation
              :inner-delim inner :entry-delim entry}))
    [inner entry]))

(defn- validate-struct-representation! [name body]
  (let [representation (get body "representation" {"map" {}})
        [strategy details] (one-entry! representation name)
        fields (get body "fields")]
    (case strategy
      "map"
      (let [wire-names (map (fn [[field _]]
                              (get-in details ["fields" field "rename"] field))
                            fields)]
        (when-not (= (count wire-names) (count (distinct wire-names)))
          (fail! :duplicate-representation-field {:type-name name :fields wire-names})))
      "tuple"
      (do
        (struct-field-order! body details strategy)
        (when (some (fn [[field spec]]
                      (or (true? (get spec "optional"))
                          (contains? (get-in body ["representation" "map" "fields" field] {})
                                     "implicit")))
                    fields)
          (fail! :tuple-optional-not-supported {:type-name name})))
      "stringjoin"
      (do
        (when-not (and (string? (get details "join"))
                       (pos? (count (get details "join"))))
          (fail! :invalid-stringjoin-delimiter {:type-name name}))
        (struct-field-order! body details strategy)
        (when (some (fn [[_ spec]] (true? (get spec "optional"))) fields)
          (fail! :stringjoin-optional-not-supported {:type-name name})))
      "stringpairs" (delimiters! details strategy)
      "listpairs" nil
      (fail! :unsupported-struct-representation
             {:type-name name :representation representation}))))

(def union-kinds
  #{"null" "bool" "int" "float" "string" "bytes" "link" "list" "map"})

(defn- ambiguous-prefixes? [prefixes starts-with?]
  (boolean
   (some true?
         (for [a prefixes b prefixes :when (not= a b)]
           (starts-with? b a)))))

(defn- validate-union-representation! [name body]
  (let [representation (get body "representation")
        [strategy details] (one-entry! representation name)
        members (get body "members")
        table (case strategy
                ("keyed" "kinded") details
                ("envelope" "inline") (get details "discriminantTable")
                ("stringprefix" "bytesprefix") (get details "prefixes")
                nil)]
    (when-not (contains? #{"keyed" "kinded" "envelope" "inline"
                           "stringprefix" "bytesprefix"} strategy)
      (fail! :unsupported-union-representation
             {:type-name name :representation representation}))
    (when-not (and (= (count members) (count (distinct members)))
                   (map? table) (every? string? (keys table))
                   (= (count table) (count members))
                   (= (set (vals table)) (set members)))
      (fail! :invalid-union-table {:type-name name :strategy strategy}))
    (when (and (= strategy "kinded")
               (not (every? union-kinds (keys table))))
      (fail! :invalid-union-kind {:type-name name :kinds (set (keys table))}))
    (when (contains? #{"envelope" "inline"} strategy)
      (let [disc (get details "discriminantKey")
            content (get details "contentKey")]
        (when-not (and (string? disc) (pos? (count disc))
                       (or (= strategy "inline")
                           (and (string? content) (pos? (count content))
                                (not= disc content))))
          (fail! :invalid-union-envelope-keys {:type-name name :strategy strategy}))))
    (when (= strategy "stringprefix")
      (let [prefixes (keys table)]
        (when (or (some empty? prefixes)
                  (ambiguous-prefixes? prefixes str/starts-with?))
          (fail! :ambiguous-union-prefix-table {:type-name name :strategy strategy}))))
    (when (= strategy "bytesprefix")
      (let [prefixes (mapv hex-bytes (keys table))]
        (when (ambiguous-prefixes? prefixes starts-with-bytes?)
          (fail! :ambiguous-union-prefix-table {:type-name name :strategy strategy}))))))

(defn- validate-representation! [name definition]
  (let [[kind body] (one-entry! definition name)]
    (case kind
      "struct" (validate-struct-representation! name body)
      "union" (validate-union-representation! name body)
      "map" (when-let [details (get-in body ["representation" "stringpairs"])]
              (delimiters! details "stringpairs"))
      nil)))

(defn- string-representable? [types ref seen]
  (let [definition (if (string? ref) (get types ref) ref)
        [kind body] (one-entry! definition :string-representation)]
    (case kind
      ("string" "bool" "int" "float") true
      "enum" (contains? (get body "representation" {"string" {}}) "string")
      "copy" (let [next-ref (get body "fromType")]
               (and (not (contains? seen next-ref))
                    (string-representable? types next-ref (conj seen next-ref))))
      false)))

(defn- representation-kind [types ref seen]
  (let [definition (if (string? ref) (get types ref) ref)
        [kind body] (one-entry! definition :representation-kind)
        representation (get body "representation")]
    (case kind
      ("bool" "string" "int" "float" "link") kind
      "bytes" (when-not (contains? representation "advanced") "bytes")
      "copy" (let [next-ref (get body "fromType")]
               (when-not (contains? seen next-ref)
                 (representation-kind types next-ref (conj seen next-ref))))
      "list" (when-not (contains? representation "advanced") "list")
      "map" (cond (contains? representation "advanced") nil
                  (contains? representation "listpairs") "list"
                  (contains? representation "stringpairs") "string"
                  :else "map")
      "struct" (cond (or (nil? representation) (contains? representation "map")) "map"
                     (or (contains? representation "tuple")
                         (contains? representation "listpairs")) "list"
                     (or (contains? representation "stringpairs")
                         (contains? representation "stringjoin")) "string")
      "union" (cond (or (contains? representation "keyed")
                         (contains? representation "envelope")
                         (contains? representation "inline")) "map"
                    (contains? representation "stringprefix") "string"
                    (contains? representation "bytesprefix") "bytes")
      "enum" (if (contains? representation "int") "int" "string")
      "unit" (case (get body "representation")
               "null" "null" ("true" "false") "bool" "emptymap" "map")
      nil)))

(defn- validate-union-member-kinds! [name definition types]
  (let [[kind body] (one-entry! definition name)]
    (when (= kind "union")
      (let [representation (get body "representation")]
        (cond
          (contains? representation "kinded")
          (doseq [[expected member] (get representation "kinded")]
            (when-not (= expected (representation-kind types member #{}))
              (fail! :union-member-kind-mismatch
                     {:type-name name :member member :expected expected})))

          (contains? representation "stringprefix")
          (doseq [member (vals (get-in representation ["stringprefix" "prefixes"]))]
            (when-not (= "string" (representation-kind types member #{}))
              (fail! :union-member-kind-mismatch
                     {:type-name name :member member :expected "string"})))

          (contains? representation "bytesprefix")
          (doseq [member (vals (get-in representation ["bytesprefix" "prefixes"]))]
            (when-not (= "bytes" (representation-kind types member #{}))
              (fail! :union-member-kind-mismatch
                     {:type-name name :member member :expected "bytes"})))

          (contains? representation "inline")
          (doseq [member (vals (get-in representation ["inline" "discriminantTable"]))]
            (when-not (= "map" (representation-kind types member #{}))
              (fail! :union-member-kind-mismatch
                     {:type-name name :member member :expected "map"}))))))))

(defn- validate-string-representations! [name definition types]
  (let [[kind body] (one-entry! definition name)
        representation (get body "representation")]
    (case kind
      "map"
      (when (contains? representation "stringpairs")
        (when (or (true? (get body "valueNullable"))
                  (not (string-representable? types (get body "keyType") #{}))
                  (not (string-representable? types (get body "valueType") #{})))
          (fail! :type-has-no-string-representation {:type-name name})))

      "struct"
      (when (or (contains? representation "stringpairs")
                (contains? representation "stringjoin"))
        (doseq [[field spec] (get body "fields")]
          (when (or (true? (get spec "nullable"))
                    (not (string-representable? types (get spec "type") #{})))
            (fail! :type-has-no-string-representation
                   {:type-name name :field field}))))
      nil)))

(declare implicit-value! unify-ref!)

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
    (let [all-types (merge prelude types)
          compiled {:dmt dmt :types all-types :advanced (set (keys advanced))}]
      (doseq [[name definition] types]
        (validate-representation! name definition)
        (validate-string-representations! name definition all-types)
        (validate-union-member-kinds! name definition all-types))
      (doseq [[name definition] types
              reference (type-refs definition)]
        (when-not (contains? all-types reference)
          (fail! :unknown-type-reference {:type-name name :reference reference})))
      (doseq [[name definition] types
              adl (adl-refs definition)]
        (when-not (contains? advanced adl)
          (fail! :unknown-advanced-reference {:type-name name :advanced adl})))
      (doseq [[name definition] types
              :let [[kind body] (one-entry! definition name)]
              :when (= kind "struct")
              [field spec] (get body "fields")
              :let [details (get-in body ["representation" "map" "fields" field] {})]
              :when (contains? details "implicit")]
        (let [state {:max-depth 32 :max-nodes 64 :nodes (atom 0)
                     :adl-validators {}}
              typed (implicit-value! compiled (get spec "type")
                                     (get details "implicit") [name field])]
          (unify-ref! compiled state (get spec "type") typed 0 [name field])))
      compiled)))

(defn- positive-limit! [limits key]
  (let [n (get limits key)]
    (when-not (and (integer? n) (pos? n))
      (fail! :positive-limit-required {:limit key :value n})) n))

(defn- adl-capabilities [limits]
  (merge-with merge
              (into {} (map (fn [[name validator]]
                              [name {:validate-representation validator}])
                            (or (:adl-validators limits) {})))
              (or (:adl-capabilities limits) {})))

(defn- adl-capability! [capabilities name operation path]
  (let [capability (get-in capabilities [name operation])]
    (when-not (fn? capability)
      (fail! (case operation
               :validate-representation :missing-adl-validator
               :decode :missing-adl-decoder
               :encode :missing-adl-encoder
               :validate-logical :missing-adl-logical-validator)
             {:adl name :path path}))
    capability))

(defn- invoke-adl! [capability name operation value path]
  (try
    (capability value)
    (catch #?(:clj Exception :cljs :default) error
      (fail! :adl-capability-failed
             {:adl name :operation operation :path path
              :message #?(:clj (.getMessage error) :cljs (.-message error))}))))

(declare consume!)

(defn- consume-data-children! [state value depth path]
  (cond
    (map? value)
    (doseq [[key item] value]
      (consume! state (inc depth) (conj path key :key))
      (consume-data-children! state key (inc depth) (conj path key :key))
      (consume! state (inc depth) (conj path key))
      (consume-data-children! state item (inc depth) (conj path key)))

    (sequential? value)
    (doseq [[i item] (map-indexed vector value)]
      (consume! state (inc depth) (conj path i))
      (consume-data-children! state item (inc depth) (conj path i)))))

(defn- validate-adl-representation! [state name value depth path]
  (consume-data-children! state value depth path)
  (let [validator (adl-capability! (:adl-capabilities state) name
                                   :validate-representation path)]
    (when-not (invoke-adl! validator name :validate-representation value path)
      (fail! :adl-rejected {:adl name :path path}))))

(defn- consume! [state depth path]
  (when (> depth (:max-depth state))
    (fail! :max-depth-exceeded {:path path :max-depth (:max-depth state)}))
  (let [n (swap! (:nodes state) inc)]
    (when (> n (:max-nodes state))
      (fail! :max-nodes-exceeded {:path path :max-nodes (:max-nodes state)}))))

(declare unify-ref!)

(defn- parse-int-text! [text path]
  (when-not (re-matches #"-?(0|[1-9][0-9]*)" text)
    (fail! :invalid-integer-text {:path path :value text}))
  (try
    #?(:clj (Long/parseLong text)
       :cljs (let [n (js/Number text)]
               (when-not (js/Number.isSafeInteger n)
                 (fail! :integer-out-of-range {:path path :value text}))
               n))
    (catch #?(:clj NumberFormatException :cljs :default) _
      (fail! :integer-out-of-range {:path path :value text}))))

(defn- parse-float-text! [text path]
  (when-not (re-matches #"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?" text)
    (fail! :invalid-float-text {:path path :value text}))
  (let [n #?(:clj (Double/parseDouble text) :cljs (js/Number text))]
    (when #?(:clj (or (Double/isInfinite n) (Double/isNaN n))
             :cljs (not (js/Number.isFinite n)))
      (fail! :invalid-float-text {:path path :value text}))
    n))

(defn- text-value! [compiled ref text path]
  (let [definition (if (string? ref) (get-in compiled [:types ref]) ref)
        [kind body] (one-entry! definition path)]
    (case kind
      "copy" (text-value! compiled (get body "fromType") text path)
      "string" text
      "bool" (case text "true" true "false" false
                    (fail! :invalid-boolean-text {:path path :value text}))
      "int" (parse-int-text! text path)
      "float" (parse-float-text! text path)
      "enum" (if (contains? (get body "representation") "string")
               text
               (fail! :type-has-no-string-representation {:path path :kind kind}))
      (fail! :type-has-no-string-representation {:path path :kind kind}))))

(defn- unify-text-ref! [compiled state ref text depth path]
  (unify-ref! compiled state ref (text-value! compiled ref text path) depth path))

(defn- implicit-value! [compiled ref raw path]
  (if (string? raw)
    (text-value! compiled ref raw path)
    raw))

(defn- split-literal [text delimiter]
  (when-not (string? text) (fail! :expected-string {:value text}))
  (if (empty? text)
    []
    (loop [start 0 out []]
      (if-let [at (str/index-of text delimiter start)]
        (recur (+ at (count delimiter)) (conj out (subs text start at)))
        (conj out (subs text start))))))

(defn- parse-string-pairs! [value details path]
  (when-not (string? value)
    (fail! :expected-stringpairs {:path path :actual (data-kind value)}))
  (let [[inner entry] (delimiters! details "stringpairs")]
    (mapv (fn [[i item]]
            (let [parts (split-literal item inner)]
              (when-not (= 2 (count parts))
                (fail! :invalid-stringpair {:path (conj path i) :value item}))
              parts))
          (map-indexed vector (split-literal value entry)))))

(defn- ensure-unique-keys! [pairs path]
  (let [keys* (map first pairs)]
    (when-not (= (count keys*) (count (distinct keys*)))
      (fail! :duplicate-pair-key {:path path}))))

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

      (contains? representation "stringpairs")
      (let [pairs (parse-string-pairs! value (get representation "stringpairs") path)]
        (ensure-unique-keys! pairs path)
        (doseq [[i [k v]] (map-indexed vector pairs)]
          (unify-text-ref! compiled state (get body "keyType") k
                           (inc depth) (conj path i 0))
          (when (true? (get body "valueNullable"))
            (fail! :nullable-has-no-string-form {:path (conj path i 1)}))
          (unify-text-ref! compiled state (get body "valueType") v
                           (inc depth) (conj path i 1))))

      (contains? representation "advanced")
      (validate-adl-representation! state (get representation "advanced") value depth path)

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
          (let [wire-value (get value wire-key)]
            (nullable-ref! compiled state (get spec "type")
                           (true? (get spec "nullable")) wire-value
                           (inc depth) (conj path wire-key))
            (when (and implicit?
                       (= wire-value
                          (implicit-value! compiled (get spec "type")
                                           (get-in field-details [field "implicit"])
                                           (conj path wire-key))))
              (fail! :explicit-implicit-value {:path (conj path wire-key)}))))))))

(defn- struct-representation! [compiled state body value depth path]
  (let [representation (get body "representation" {"map" {}})]
    (cond
      (contains? representation "map")
      (struct-map! compiled state body (get representation "map") value depth path)

      (contains? representation "tuple")
      (let [details (get representation "tuple")
            order (struct-field-order! body details "tuple")
            fields (mapv (fn [field] [field (get-in body ["fields" field])]) order)]
        (when-not (and (sequential? value) (= (count fields) (count value)))
          (fail! :invalid-struct-tuple {:path path :expected (count fields)}))
        (doseq [[[field spec] item] (map vector fields value)]
          (nullable-ref! compiled state (get spec "type")
                         (true? (get spec "nullable")) item
                         (inc depth) (conj path field))))

      (contains? representation "stringpairs")
      (let [pairs (parse-string-pairs! value (get representation "stringpairs") path)
            fields (get body "fields")]
        (ensure-unique-keys! pairs path)
        (doseq [[field _] pairs]
          (when-not (contains? fields field)
            (fail! :unknown-struct-field {:path (conj path field)})))
        (doseq [[field spec] fields]
          (let [entry (first (filter #(= field (first %)) pairs))]
            (when (and (nil? entry) (not (true? (get spec "optional"))))
              (fail! :missing-struct-field {:path (conj path field)}))
            (when entry
              (when (true? (get spec "nullable"))
                (fail! :nullable-has-no-string-form {:path (conj path field)}))
              (unify-text-ref! compiled state (get spec "type") (second entry)
                               (inc depth) (conj path field))))))

      (contains? representation "listpairs")
      (let [fields (get body "fields")]
        (when-not (sequential? value) (fail! :expected-listpairs {:path path}))
        (let [pairs (mapv (fn [[i pair]]
                            (when-not (and (sequential? pair) (= 2 (count pair))
                                           (string? (first pair)))
                              (fail! :invalid-listpair {:path (conj path i)}))
                            (vec pair))
                          (map-indexed vector value))]
          (ensure-unique-keys! pairs path)
          (doseq [[field _] pairs]
            (when-not (contains? fields field)
              (fail! :unknown-struct-field {:path (conj path field)})))
          (doseq [[field spec] fields]
            (let [entry (first (filter #(= field (first %)) pairs))]
              (when (and (nil? entry) (not (true? (get spec "optional"))))
                (fail! :missing-struct-field {:path (conj path field)}))
              (when entry
                (nullable-ref! compiled state (get spec "type")
                               (true? (get spec "nullable")) (second entry)
                               (inc depth) (conj path field)))))))

      (contains? representation "stringjoin")
      (let [details (get representation "stringjoin")
            order (struct-field-order! body details "stringjoin")
            values (split-literal value (get details "join"))]
        (when-not (= (count order) (count values))
          (fail! :invalid-stringjoin-arity
                 {:path path :expected (count order) :actual (count values)}))
        (doseq [[field item] (map vector order values)]
          (let [spec (get-in body ["fields" field])]
            (when (true? (get spec "nullable"))
              (fail! :nullable-has-no-string-form {:path (conj path field)}))
            (unify-text-ref! compiled state (get spec "type") item
                             (inc depth) (conj path field)))))

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
                  (validate-adl-representation! state advanced value depth path)
                  (when-not (bytes-like? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)})))
      "int" (when-not (integer? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "float" (when-not (and (number? value) (not (integer? value)))
                  (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "link" (when-not (link/link? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "copy" (unify-ref! compiled state (get body "fromType") value (inc depth) path)
      "list" (if-let [advanced (get-in body ["representation" "advanced"])]
               (validate-adl-representation! state advanced value depth path)
               (do (when-not (sequential? value) (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
                   (doseq [[i item] (map-indexed vector value)]
                     (nullable-ref! compiled state (get body "valueType")
                                    (true? (get body "valueNullable")) item
                                    (inc depth) (conj path i)))))
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
  `:adl-capabilities` supplies caller-owned ADL operations; legacy
  `:adl-validators` remains supported for representation predicates."
  [compiled type-name value limits]
  (let [state {:max-depth (positive-limit! limits :max-depth)
               :max-nodes (positive-limit! limits :max-nodes)
               :nodes (atom 0)
               :adl-capabilities (adl-capabilities limits)}]
    (unify-ref! compiled state type-name value 0 [])
    {:type type-name :nodes @(:nodes state) :value value}))

(declare decode-ref encode-ref)

(defn- decode-nullable [compiled capabilities ref nullable? value path]
  (if (nil? value)
    (if nullable? nil (fail! :unexpected-null {:path path}))
    (decode-ref compiled capabilities ref value path)))

(defn- decode-adl [capabilities name value path]
  (let [decoder (adl-capability! capabilities name :decode path)
        logical (invoke-adl! decoder name :decode value path)
        logical-validator (get-in capabilities [name :validate-logical])]
    (when (and logical-validator
               (not (invoke-adl! logical-validator name :validate-logical logical path)))
      (fail! :adl-logical-rejected {:adl name :path path}))
    logical))

(defn- decode-map [compiled capabilities body value path]
  (let [representation (get body "representation")
        pairs (cond
                (or (nil? representation) (contains? representation "map")) value
                (contains? representation "listpairs") value
                (contains? representation "stringpairs")
                (parse-string-pairs! value (get representation "stringpairs") path)
                (contains? representation "advanced")
                ::advanced
                :else (fail! :projection-not-supported
                             {:path path :representation representation}))]
    (if (= ::advanced pairs)
      (decode-adl capabilities (get representation "advanced") value path)
      (into {}
            (map-indexed
             (fn [i [k v]]
               [(if (contains? representation "stringpairs")
                  (text-value! compiled (get body "keyType") k (conj path i 0))
                  (decode-ref compiled capabilities (get body "keyType") k (conj path i 0)))
                (if (contains? representation "stringpairs")
                  (text-value! compiled (get body "valueType") v (conj path i 1))
                  (decode-nullable compiled capabilities (get body "valueType")
                                   (true? (get body "valueNullable")) v
                                   (conj path i 1)))])
             pairs)))))

(defn- decode-struct [compiled capabilities body value path]
  (let [representation (get body "representation" {"map" {}})
        fields (get body "fields")]
    (cond
      (contains? representation "map")
      (let [details (get representation "map")]
        (into {}
              (keep (fn [[field spec]]
                      (let [wire (get-in details ["fields" field "rename"] field)
                            configured (get-in details ["fields" field] {})]
                        (cond
                          (contains? value wire)
                          [field (decode-nullable compiled capabilities (get spec "type")
                                                  (true? (get spec "nullable"))
                                                  (get value wire) (conj path wire))]
                          (contains? configured "implicit")
                          [field (implicit-value! compiled (get spec "type")
                                                  (get configured "implicit")
                                                  (conj path field))]
                          :else nil)))
                    fields)))

      (contains? representation "tuple")
      (let [order (struct-field-order! body (get representation "tuple") "tuple")]
        (into {} (map (fn [field item]
                        (let [spec (get fields field)]
                          [field (decode-nullable compiled capabilities (get spec "type")
                                                  (true? (get spec "nullable")) item
                                                  (conj path field))]))
                      order value)))

      (contains? representation "stringpairs")
      (into {}
            (map (fn [[field text]]
                   [field (text-value! compiled (get-in fields [field "type"])
                                       text (conj path field))]))
            (parse-string-pairs! value (get representation "stringpairs") path))

      (contains? representation "listpairs")
      (into {}
            (map (fn [[field item]]
                   (let [spec (get fields field)]
                     [field (decode-nullable compiled capabilities (get spec "type")
                                             (true? (get spec "nullable")) item
                                             (conj path field))])))
            value)

      (contains? representation "stringjoin")
      (let [details (get representation "stringjoin")
            order (struct-field-order! body details "stringjoin")
            values (split-literal value (get details "join"))]
        (into {} (map (fn [field text]
                        [field (text-value! compiled (get-in fields [field "type"])
                                            text (conj path field))])
                      order values)))

      :else value)))

(defn- decode-enum [body value path]
  (let [members (get body "members")
        representation (get body "representation" {"string" {}})
        [strategy mapping] (one-entry! representation path)]
    (or (some (fn [member]
                (when (= value (get mapping member member)) member))
              members)
        (fail! :invalid-enum-value {:path path :value value :representation strategy}))))

(defn- decode-union [compiled capabilities body value path]
  (let [representation (get body "representation")
        [member content content-path]
        (cond
          (contains? representation "keyed")
          (let [[discriminant content] (first value)]
            [(get-in representation ["keyed" discriminant]) content
             (conj path discriminant)])

          (contains? representation "kinded")
          [(get-in representation ["kinded" (data-kind value)]) value path]

          (contains? representation "envelope")
          (let [{disc-key "discriminantKey" content-key "contentKey"
                 table "discriminantTable"} (get representation "envelope")]
            [(get table (get value disc-key)) (get value content-key)
             (conj path content-key)])

          (contains? representation "inline")
          (let [{disc-key "discriminantKey" table "discriminantTable"}
                (get representation "inline")]
            [(get table (get value disc-key)) (dissoc value disc-key) path])

          (contains? representation "stringprefix")
          (let [[prefix member]
                (first (filter (fn [[prefix _]] (str/starts-with? value prefix))
                               (get-in representation ["stringprefix" "prefixes"])))]
            [member (subs value (count prefix)) path])

          (contains? representation "bytesprefix")
          (let [octets (byte-vector value)
                [prefix member]
                (first (keep (fn [[prefix member]]
                               (let [prefix-bytes (hex-bytes prefix)]
                                 (when (starts-with-bytes? octets prefix-bytes)
                                   [prefix-bytes member])))
                             (get-in representation ["bytesprefix" "prefixes"])))]
            [member (bytes-from (drop (count prefix) octets)) path])

          :else (fail! :projection-not-supported
                       {:path path :representation representation}))]
    {:member member
     :value (decode-ref compiled capabilities member content content-path)}))

(defn- decode-ref [compiled capabilities ref value path]
  (let [definition (if (string? ref) (get-in compiled [:types ref]) ref)
        [kind body] (one-entry! definition path)]
    (case kind
      "copy" (decode-ref compiled capabilities (get body "fromType") value path)
      "list" (if-let [advanced (get-in body ["representation" "advanced"])]
               (decode-adl capabilities advanced value path)
               (mapv (fn [i item]
                       (decode-nullable compiled capabilities (get body "valueType")
                                        (true? (get body "valueNullable")) item
                                        (conj path i)))
                     (range) value))
      "map" (decode-map compiled capabilities body value path)
      "struct" (decode-struct compiled capabilities body value path)
      "union" (decode-union compiled capabilities body value path)
      "enum" (decode-enum body value path)
      "bytes" (if-let [advanced (get-in body ["representation" "advanced"])]
                (decode-adl capabilities advanced value path)
                value)
      value)))

(defn representation->logical!
  "Validate a representation value, then return its logical typed value.
  Struct field names and typed implicit values are restored."
  [compiled type-name value limits]
  (let [result (unify! compiled type-name value limits)
        capabilities (adl-capabilities limits)]
    (assoc result :logical-value
           (decode-ref compiled capabilities type-name value []))))

(defn- encode-text! [compiled ref value path]
  (let [definition (if (string? ref) (get-in compiled [:types ref]) ref)
        [kind body] (one-entry! definition path)]
    (case kind
      "copy" (encode-text! compiled (get body "fromType") value path)
      "string" (if (string? value) value
                    (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "bool" (if (boolean? value) (if value "true" "false")
                  (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "int" (if (integer? value) (str value)
                 (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "float" (if (number? value) (str value)
                   (fail! :kind-mismatch {:path path :expected kind :actual (data-kind value)}))
      "enum" (let [mapping (get-in body ["representation" "string"] ::missing)]
               (when (= ::missing mapping)
                 (fail! :type-has-no-string-representation {:path path :kind kind}))
               (if (contains? (set (get body "members")) value)
                 (str (get mapping value value))
                 (fail! :invalid-enum-value {:path path :value value})))
      (fail! :type-has-no-string-representation {:path path :kind kind}))))

(defn- no-delimiter! [text delimiters path]
  (when (some #(str/includes? text %) delimiters)
    (fail! :unescaped-delimiter {:path path :value text :delimiters delimiters}))
  text)

(defn- encode-nullable [compiled state ref nullable? value depth path]
  (if (nil? value)
    (if nullable? nil (fail! :unexpected-null {:path path}))
    (encode-ref compiled state ref value depth path)))

(defn- encode-adl [state name logical path]
  (let [capabilities (:adl-capabilities state)
        logical-validator (get-in capabilities [name :validate-logical])]
    (when (and logical-validator
               (not (invoke-adl! logical-validator name :validate-logical logical path)))
      (fail! :adl-logical-rejected {:adl name :path path}))
    (invoke-adl! (adl-capability! capabilities name :encode path)
                 name :encode logical path)))

(defn- logical-fields! [body value path]
  (when-not (and (map? value) (every? string? (keys value)))
    (fail! :expected-logical-struct {:path path}))
  (let [fields (get body "fields")]
    (doseq [field (keys value)]
      (when-not (contains? fields field)
        (fail! :unknown-struct-field {:path (conj path field)})))
    fields))

(defn- encode-struct [compiled state body value depth path]
  (let [fields (logical-fields! body value path)
        representation (get body "representation" {"map" {}})]
    (cond
      (contains? representation "map")
      (let [details (get representation "map")]
        (into {}
              (keep (fn [[field spec]]
                      (let [wire (get-in details ["fields" field "rename"] field)
                            configured (get-in details ["fields" field] {})
                            present? (contains? value field)]
                        (cond
                          (and (not present?) (true? (get spec "optional"))) nil
                          (not present?) (fail! :missing-struct-field {:path (conj path field)})
                          (and (contains? configured "implicit")
                               (= (get value field)
                                  (implicit-value! compiled (get spec "type")
                                                   (get configured "implicit")
                                                   (conj path field)))) nil
                          :else [wire (encode-nullable compiled state (get spec "type")
                                                       (true? (get spec "nullable"))
                                                       (get value field) (inc depth)
                                                       (conj path field))]))))
                    fields))

      (contains? representation "tuple")
      (mapv (fn [field]
              (when-not (contains? value field)
                (fail! :missing-struct-field {:path (conj path field)}))
              (let [spec (get fields field)]
                (encode-nullable compiled state (get spec "type")
                                 (true? (get spec "nullable")) (get value field)
                                 (inc depth) (conj path field))))
            (struct-field-order! body (get representation "tuple") "tuple"))

      (contains? representation "listpairs")
      (into []
            (keep (fn [[field spec]]
                    (cond
                      (contains? value field)
                      [field (encode-nullable compiled state (get spec "type")
                                              (true? (get spec "nullable"))
                                              (get value field) (inc depth)
                                              (conj path field))]
                      (true? (get spec "optional")) nil
                      :else (fail! :missing-struct-field {:path (conj path field)}))))
            fields)

      (contains? representation "stringpairs")
      (let [details (get representation "stringpairs")
            [inner entry] (delimiters! details "stringpairs")]
        (str/join entry
                  (keep (fn [[field spec]]
                          (cond
                            (contains? value field)
                            (let [text (encode-text! compiled (get spec "type")
                                                     (get value field) (conj path field))]
                              (str (no-delimiter! field [inner entry] (conj path field :key))
                                   inner
                                   (no-delimiter! text [inner entry] (conj path field))))
                            (true? (get spec "optional")) nil
                            :else (fail! :missing-struct-field {:path (conj path field)})))
                        fields)))

      (contains? representation "stringjoin")
      (let [details (get representation "stringjoin")
            join (get details "join")]
        (str/join join
                  (map (fn [field]
                         (when-not (contains? value field)
                           (fail! :missing-struct-field {:path (conj path field)}))
                         (no-delimiter!
                          (encode-text! compiled (get-in fields [field "type"])
                                        (get value field) (conj path field))
                          [join] (conj path field)))
                       (struct-field-order! body details "stringjoin"))))

      :else (fail! :projection-not-supported {:path path :representation representation}))))

(defn- encode-map [compiled state body value depth path]
  (let [representation (get body "representation")]
    (cond
      (contains? representation "advanced")
      (encode-adl state (get representation "advanced") value path)

      (contains? representation "stringpairs")
      (let [_ (when-not (map? value) (fail! :expected-logical-map {:path path}))
            [inner entry] (delimiters! (get representation "stringpairs") "stringpairs")]
        (str/join entry
                  (map (fn [[kt vt]] (str kt inner vt))
                       (sort-by first
                                (map-indexed
                                 (fn [i [k v]]
                                   [(no-delimiter!
                                     (encode-text! compiled (get body "keyType") k
                                                   (conj path i 0))
                                     [inner entry] (conj path i 0))
                                    (no-delimiter!
                                     (encode-text! compiled (get body "valueType") v
                                                   (conj path i 1))
                                     [inner entry] (conj path i 1))])
                                 value)))))

      (or (nil? representation) (contains? representation "map")
          (contains? representation "listpairs"))
      (let [_ (when-not (map? value) (fail! :expected-logical-map {:path path}))
            pairs (mapv (fn [[k v]]
                          [(encode-ref compiled state (get body "keyType") k
                                       (inc depth) (conj path k :key))
                           (encode-nullable compiled state (get body "valueType")
                                            (true? (get body "valueNullable")) v
                                            (inc depth) (conj path k))])
                        value)]
        (if (contains? representation "listpairs") pairs (into {} pairs)))

      :else (fail! :projection-not-supported {:path path :representation representation}))))

(defn- encode-enum [body value path]
  (when-not (contains? (set (get body "members")) value)
    (fail! :invalid-enum-member {:path path :value value}))
  (let [[_ mapping] (one-entry! (get body "representation" {"string" {}}) path)]
    (get mapping value value)))

(defn- inverse-member! [table member path]
  (let [matches (filter (fn [[_ candidate]] (= member candidate)) table)]
    (when-not (= 1 (count matches))
      (fail! :union-member-not-represented
             {:path path :member member :matches (count matches)}))
    (ffirst matches)))

(defn- encode-union [compiled state body logical depth path]
  (when-not (and (map? logical) (= #{:member :value} (set (keys logical))))
    (fail! :invalid-logical-union {:path path :value logical}))
  (let [member (:member logical)
        _ (when-not (some #(= member %) (get body "members"))
            (fail! :unknown-union-member {:path path :member member}))
        content (encode-ref compiled state member (:value logical) (inc depth)
                            (conj path :value))
        representation (get body "representation")]
    (cond
      (contains? representation "keyed")
      {(inverse-member! (get representation "keyed") member path) content}

      (contains? representation "kinded")
      (let [kind (inverse-member! (get representation "kinded") member path)]
        (when-not (= kind (data-kind content))
          (fail! :union-member-kind-mismatch
                 {:path path :member member :expected kind :actual (data-kind content)}))
        content)

      (contains? representation "envelope")
      (let [{disc-key "discriminantKey" content-key "contentKey"
             table "discriminantTable"} (get representation "envelope")]
        {disc-key (inverse-member! table member path) content-key content})

      (contains? representation "inline")
      (let [{disc-key "discriminantKey" table "discriminantTable"}
            (get representation "inline")]
        (when-not (and (map? content) (every? string? (keys content))
                       (not (contains? content disc-key)))
          (fail! :invalid-inline-union-content {:path path :member member}))
        (assoc content disc-key (inverse-member! table member path)))

      (contains? representation "stringprefix")
      (do
        (when-not (string? content)
          (fail! :expected-string-prefix-union-content {:path path :member member}))
        (str (inverse-member! (get-in representation ["stringprefix" "prefixes"])
                              member path)
             content))

      (contains? representation "bytesprefix")
      (do
        (when-not (bytes-like? content)
          (fail! :expected-bytes-prefix-union-content {:path path :member member}))
        (bytes-from
         (concat (hex-bytes
                  (inverse-member! (get-in representation ["bytesprefix" "prefixes"])
                                   member path))
                 (byte-vector content))))

      :else (fail! :projection-not-supported {:path path :representation representation}))))

(defn- encode-ref [compiled state ref value depth path]
  (consume! state depth path)
  (let [definition (if (string? ref) (get-in compiled [:types ref]) ref)
        [kind body] (one-entry! definition path)]
    (case kind
      "copy" (encode-ref compiled state (get body "fromType") value (inc depth) path)
      "list" (if-let [advanced (get-in body ["representation" "advanced"])]
               (encode-adl state advanced value path)
               (do
                 (when-not (sequential? value) (fail! :expected-logical-list {:path path}))
                 (mapv (fn [i item]
                         (encode-nullable compiled state (get body "valueType")
                                          (true? (get body "valueNullable")) item
                                          (inc depth) (conj path i)))
                       (range) value)))
      "map" (encode-map compiled state body value depth path)
      "struct" (encode-struct compiled state body value depth path)
      "union" (encode-union compiled state body value depth path)
      "enum" (encode-enum body value path)
      "bytes" (if-let [advanced (get-in body ["representation" "advanced"])]
                (encode-adl state advanced value path)
                value)
      value)))

(defn logical->representation!
  "Project a logical typed value to its IPLD Data Model representation.
  The projected value is validated under the same mandatory resource limits."
  [compiled type-name logical-value limits]
  (let [state {:max-depth (positive-limit! limits :max-depth)
               :max-nodes (positive-limit! limits :max-nodes)
               :nodes (atom 0)
               :adl-capabilities (adl-capabilities limits)}
        value (encode-ref compiled state type-name logical-value 0 [])
        checked (unify! compiled type-name value limits)]
    (assoc checked :logical-value logical-value)))

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
