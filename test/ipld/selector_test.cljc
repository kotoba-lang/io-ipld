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
