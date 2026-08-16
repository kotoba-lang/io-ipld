(ns ipld.schema-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [ipld.schema :as schema]
            [ipld.schema-dsl :as dsl]))

(def official-example
  "type ExampleWithNullable {String : nullable &Any}

type ExampleWithAnonDefns struct {
  fooField optional {String:String} (rename \"foo_field\")
  barField nullable {String:String}
  bazField {String : nullable String}
  wozField {String:[nullable String]}
  boomField &ExampleWithNullable
} representation map

type ExampleOfUnit unit representation null
type ExampleOfAny any")

(def application-schema
  "# comments and whitespace do not enter the DMT
advanced Ciphertext
type Person struct {
  name String
  nick optional nullable String
} representation map
type PersonTuple struct {
  name String
  age Int
} representation tuple
type Event union {
  | Person \"person\"
  | String \"note\"
} representation keyed
type Scalar union {
  | String string
  | Int int
} representation kinded
type Envelope union {
  | Person \"person\"
  | String \"note\"
} representation envelope {
  discriminantKey \"kind\"
  contentKey \"value\"
}
type Error struct { message String }
type Inline union {
  | Error \"error\"
} representation inline { discriminantKey \"kind\" }
type Username string
type Authorization union {
  | Username \"user:\"
  | String \"text:\"
} representation stringprefix
type Rsa bytes
type Ed bytes
type PublicKey union {
  | Rsa \"00\"
  | Ed \"01\"
} representation bytesprefix
type Mode enum {
  | On (\"on\")
  | Off (\"off\")
} representation string
type Names [String]
type Labels {String:String}
type Alias = Person
type Secret bytes representation advanced Ciphertext")

(def limits {:max-depth 32 :max-nodes 256})

(def representation-schema
  "type Reordered struct {
  first String
  second Int
} representation tuple { fieldOrder [\"second\", \"first\"] }
type Joined struct {
  enabled Bool
  count Int
  label String
} representation stringjoin { join \":\" fieldOrder [\"label\", \"count\", \"enabled\"] }
type StringFields struct {
  enabled Bool
  count Int
  note optional String
} representation stringpairs { innerDelim \"=\" entryDelim \",\" }
type ListFields struct {
  name String
  count Int
  note optional String
} representation listpairs
type StringMap {String:Int} representation stringpairs {
  innerDelim \"=\"
  entryDelim \",\"
}
type Defaults struct {
  enabled Bool (implicit \"false\")
  count Int (implicit \"0\")
  ratio Float (implicit 1.5)
  label optional String
} representation map")

(def legacy-node-schema
  {:type :union
   :discriminator "kind"
   :members
   {"leaf" {:type :struct
             :fields {"kind" {:type :kind :kind :string}
                      "entries" {:type :list
                                 :items {:type :tuple
                                         :items [{:type :kind :kind :string}
                                                 {:type :any}]}}}}
    "internal" {:type :struct
                :fields {"kind" {:type :kind :kind :string}
                         "children" {:type :list
                                     :items {:type :tuple
                                             :items [{:type :kind :kind :string}
                                                     {:type :kind :kind :link}]}}}}}})

(deftest legacy-schema-algebra-remains-compatible
  (is (some? (schema/unify "ProllyNode" legacy-node-schema
                           {"kind" "leaf" "entries" [["a" 1]]})))
  (is (= ["children" 0 1]
         (:path (schema/valid? legacy-node-schema
                               {"kind" "internal"
                                "children" [["a" "plain-cid-is-not-link"]]}))))
  (is (nil? (schema/unify "ProllyNode" legacy-node-schema
                          {"kind" "unknown"}))))

(deftest official-example-normalizes-to-schema-dmt
  (let [dmt (dsl/parse official-example)]
    (is (= {"map" {"keyType" "String"
                    "valueType" {"link" {}}
                    "valueNullable" true}}
           (get-in dmt ["types" "ExampleWithNullable"])))
    (is (= "foo_field"
           (get-in dmt ["types" "ExampleWithAnonDefns" "struct"
                        "representation" "map" "fields" "fooField" "rename"])))
    (is (= {"unit" {"representation" "null"}}
           (get-in dmt ["types" "ExampleOfUnit"])))
    (is (= {"any" {}} (get-in dmt ["types" "ExampleOfAny"])))))

(deftest dmt-compiles-and-unifies-representation-values
  (let [compiled (schema/compile-schema (dsl/parse application-schema))]
    (is (schema/valid? compiled "Person" {"name" "Ada"} limits))
    (is (schema/valid? compiled "Person" {"name" "Ada" "nick" nil} limits))
    (is (not (schema/valid? compiled "Person" {"name" "Ada" "extra" 1} limits)))
    (is (not (schema/valid? compiled "Person" {"nick" "A"} limits)))
    (is (schema/valid? compiled "PersonTuple" ["Ada" 37] limits))
    (is (schema/valid? compiled "Event" {"person" {"name" "Ada"}} limits))
    (is (schema/valid? compiled "Event" {"note" "hello"} limits))
    (is (not (schema/valid? compiled "Event" {"note" 1} limits)))
    (is (schema/valid? compiled "Scalar" "text" limits))
    (is (schema/valid? compiled "Scalar" 42 limits))
    (is (schema/valid? compiled "Envelope"
                       {"kind" "person" "value" {"name" "Ada"}} limits))
    (is (schema/valid? compiled "Inline"
                       {"kind" "error" "message" "bad"} limits))
    (is (schema/valid? compiled "Authorization" "user:ada" limits))
    (is (schema/valid? compiled "PublicKey"
                       #?(:clj (byte-array [0 2 3])
                          :cljs (js/Uint8Array.from #js [0 2 3])) limits))
    (is (schema/valid? compiled "Mode" "on" limits))
    (is (not (schema/valid? compiled "Mode" "On" limits)))
    (is (schema/valid? compiled "Names" ["a" "b"] limits))
    (is (schema/valid? compiled "Labels" {"a" "b"} limits))
    (is (schema/valid? compiled "Alias" {"name" "Ada"} limits))
    (is (schema/valid? compiled "Secret" [1 2 3]
                       (assoc limits :adl-validators {"Ciphertext" vector?})))
    (is (not (schema/valid? compiled "Secret" [1 2 3] limits)))))

(deftest references-syntax-and-resource-limits-fail-closed
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (schema/compile-schema (dsl/parse "type Broken Missing"))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (schema/compile-schema
                (dsl/parse "type Secret bytes representation advanced Missing"))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (dsl/parse "type U union { | String \"s\" } representation mystery")))
  (let [compiled (schema/compile-schema
                  (dsl/parse "type Tree struct { name String children [Tree] }"))
        value {"name" "root"
               "children" [{"name" "a" "children" []}
                            {"name" "b" "children" []}]}]
    (is (schema/valid? compiled "Tree" value limits))
    (is (not (schema/valid? compiled "Tree" value
                            {:max-depth 2 :max-nodes 256})))
    (is (not (schema/valid? compiled "Tree" value
                            {:max-depth 32 :max-nodes 3})))))

(deftest representation-families-are-bidirectional-typed-lenses
  (let [dmt (dsl/parse representation-schema)
        compiled (schema/compile-schema dmt)]
    (is (= ["second" "first"]
           (get-in dmt ["types" "Reordered" "struct"
                        "representation" "tuple" "fieldOrder"])))
    (is (= 1.5
           (get-in dmt ["types" "Defaults" "struct"
                        "representation" "map" "fields" "ratio" "implicit"])))
    (is (= {"first" "Ada" "second" 37}
           (:logical-value
            (schema/representation->logical! compiled "Reordered" [37 "Ada"] limits))))
    (is (= [37 "Ada"]
           (:value
            (schema/logical->representation! compiled "Reordered"
                                             {"first" "Ada" "second" 37} limits))))

    (is (= {"enabled" false "count" 3 "label" "job"}
           (:logical-value
            (schema/representation->logical! compiled "Joined" "job:3:false" limits))))
    (is (= "job:3:false"
           (:value
            (schema/logical->representation! compiled "Joined"
                                             {"enabled" false "count" 3 "label" "job"}
                                             limits))))

    (is (= {"enabled" true "count" 2}
           (:logical-value
            (schema/representation->logical! compiled "StringFields"
                                             "enabled=true,count=2" limits))))
    (is (= "enabled=true,count=2"
           (:value
            (schema/logical->representation! compiled "StringFields"
                                             {"enabled" true "count" 2} limits))))

    (is (= {"name" "Ada" "count" 2}
           (:logical-value
            (schema/representation->logical! compiled "ListFields"
                                             [["name" "Ada"] ["count" 2]] limits))))
    (is (= [["name" "Ada"] ["count" 2]]
           (:value
            (schema/logical->representation! compiled "ListFields"
                                             {"name" "Ada" "count" 2} limits))))

    (is (= {"a" 1 "b" 2}
           (:logical-value
            (schema/representation->logical! compiled "StringMap" "a=1,b=2" limits))))
    (is (= "a=1,b=2"
           (:value
            (schema/logical->representation! compiled "StringMap"
                                             (array-map "b" 2 "a" 1) limits))))

    (is (= {"enabled" false "count" 0 "ratio" 1.5}
           (:logical-value
            (schema/representation->logical! compiled "Defaults" {} limits))))
    (is (= {}
           (:value
            (schema/logical->representation! compiled "Defaults"
                                             {"enabled" false "count" 0 "ratio" 1.5}
                                             limits))))))

(deftest representation-ambiguity-and-invalid-configuration-fail-closed
  (let [compiled (schema/compile-schema (dsl/parse representation-schema))]
    (is (not (schema/valid? compiled "StringFields" "enabled=true,enabled=false,count=2" limits)))
    (is (not (schema/valid? compiled "StringMap" "a=01" limits)))
    (is (not (schema/valid? compiled "Defaults" {"enabled" false} limits)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (schema/logical->representation! compiled "Joined"
                                                  {"enabled" false "count" 3
                                                   "label" "bad:value"}
                                                  limits))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (schema/compile-schema
                (dsl/parse "type Bad struct { a String b Int } representation tuple { fieldOrder [\"a\", \"a\"] }"))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (schema/compile-schema
                (dsl/parse "type Bad {String:Int} representation stringpairs { innerDelim \",\" entryDelim \",\" }"))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (schema/compile-schema
                (dsl/parse "type Bad struct { nested {String:String} } representation stringjoin { join \":\" }")))))
