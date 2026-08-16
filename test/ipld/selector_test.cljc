(ns ipld.selector-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [ipld.selector :as selector]))

(deftest field-and-all-selectors
  (let [node {"a" {"v" 1} "b" {"v" 2}}]
    (is (= [{:path ["a" "v"] :value 1}]
           (selector/select node
                            {:selector :explore-fields
                             :fields {"a" {:selector :explore-fields
                                            :fields {"v" {:selector :matcher}}}}})))
    (is (= #{["a"] ["b"]}
           (set (map :path
                     (selector/select node
                                      {:selector :explore-all
                                       :next {:selector :matcher}})))))))

(deftest null-is-a-present-selectable-value
  (is (= [{:path ["value"] :value nil}]
         (selector/select {"value" nil}
                          {:selector :explore-fields
                           :fields {"value" {:selector :matcher}}}))))

(deftest compact-selector-data-model-and-dag-cbor-round-trip
  (let [executable {:selector :explore-fields
                    :fields {"child" {:selector :explore-all
                                       :next {:selector :matcher}}}}
        compact {"f" {"f>" {"child" {"a" {">" {"." {}}}}}}}]
    (is (= compact (selector/to-data-model executable)))
    (is (= executable (selector/from-data-model compact)))
    (is (= executable (selector/decode (selector/encode executable))))))

(deftest selector-wire-boundary-fails-closed
  (doseq [bad [{"." {"label" "not-supported"}}
               {"R" {"l" {"depth" 3} ":>" {"@" {}}}}
               {"a" {}}
               {"f" {"f>" {0 {"." {}}}}}
               {"." {} "a" {">" {"." {}}}}]]
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (selector/from-data-model bad))))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (selector/to-data-model {:selector :explore-fields
                                        :fields {0 {:selector :matcher}}}))))
