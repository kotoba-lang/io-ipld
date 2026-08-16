(ns ipld.schema-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [ipld.schema :as schema]))

(def node-schema
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

(deftest inline-union-unification
  (is (some? (schema/unify "ProllyNode" node-schema
                           {"kind" "leaf" "entries" [["a" 1]]})))
  (is (= ["children" 0 1]
         (:path (schema/valid? node-schema
                               {"kind" "internal"
                                "children" [["a" "plain-cid-is-not-link"]]}))))
  (is (nil? (schema/unify "ProllyNode" node-schema
                          {"kind" "unknown"}))))
