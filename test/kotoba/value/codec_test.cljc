(ns kotoba.value.codec-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [ipld.value :as implementation]
            [kotoba.value.codec :as codec]))

(defn- bytes-equal? [left right]
  (= (mapv #(bit-and % 0xff) (seq left))
     (mapv #(bit-and % 0xff) (seq right))))

(deftest facade-preserves-the-landed-wire-contract
  (is (= "kotoba.value.v1" codec/codec-id))
  (doseq [value [nil true -1 "text" :app/ready 'actor/run
                 [1 :two] '(1 :two) #{1 :two} {:a 1}]]
    (testing (pr-str value)
      (is (bytes-equal? (implementation/encode-value value)
                        (codec/encode-value value)))
      (is (= value (codec/decode-value (codec/encode-value value)))))))

(deftest facade-keeps-explicit-float-semantics
  (let [wrapped (codec/float64 0.5)
        decoded (codec/decode-value (codec/encode-value wrapped))
        implementation-form (implementation/value->form wrapped)
        facade-form (codec/value->form wrapped)]
    (is (codec/float64? decoded))
    (is (= 0.5 (codec/float64-value decoded)))
    (is (= (first implementation-form) (first facade-form)))
    (is (bytes-equal? (second implementation-form) (second facade-form)))
    (is (= 0.5
           (codec/float64-value
            (codec/form->value (codec/value->form wrapped)))))))
