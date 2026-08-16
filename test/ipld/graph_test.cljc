(ns ipld.graph-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [ipld.core :as ipld]
            [ipld.graph :as graph]))

(def limits {:max-blocks 8 :max-bytes 4096 :max-depth 8 :max-matches 8})

(defn byte-length [bytes]
  #?(:clj (alength ^bytes bytes) :cljs (.-length bytes)))

(defn fixture []
  (let [store (atom {})
        put! (fn [cid bytes] (swap! store assoc cid bytes))
        leaf (ipld/put-node! put! {"name" "leaf" "score" 1.1})
        root (ipld/put-node! put! {"child" (ipld/link leaf) "name" "root"})]
    {:store store :leaf leaf :root root
     :get-fn (fn [cid] (get @store cid))}))

(deftest selected-blocks-are-verified-root-first-and-deduplicated
  (let [{:keys [root leaf get-fn]} (fixture)
        result (graph/resolve-path get-fn root ["child" "score"] limits)]
    (is (= 1.1 (:value result)))
    (is (= [root leaf] (mapv :cid (:blocks result))))
    (is (= {:blocks 2 :bytes (reduce + (map #(byte-length (:bytes %)) (:blocks result)))
            :matches 1}
           (:stats result)))))

(deftest traversal-fails-closed-on-resource-and-integrity-boundaries
  (let [{:keys [store root leaf get-fn]} (fixture)]
    (testing "block budget"
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (graph/resolve-path get-fn root ["child" "score"]
                                       (assoc limits :max-blocks 1)))))
    (testing "missing block"
      (swap! store dissoc leaf)
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (graph/resolve-path get-fn root ["child" "score"] limits))))))

(deftest selectors-can-return-more-than-one-linked-value
  (let [{:keys [root get-fn]} (fixture)
        result (graph/select-blocks
                get-fn root
                {:selector :explore-fields
                 :fields {"name" {:selector :matcher}
                          "child" {:selector :explore-fields
                                   :fields {"name" {:selector :matcher}}}}}
                limits)]
    (is (= [["child" "name"] ["name"]] (mapv :path (:matches result))))
    (is (= ["leaf" "root"] (mapv :value (:matches result))))))
