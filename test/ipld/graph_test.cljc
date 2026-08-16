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

(deftest recursive-selector-walks-linked-dag-under-both-limits
  (let [store (atom {})
        put! (fn [cid bytes] (swap! store assoc cid bytes))
        leaf (ipld/put-node! put! {"name" "leaf"})
        middle (ipld/put-node! put! {"name" "middle" "child" (ipld/link leaf)})
        root (ipld/put-node! put! {"name" "root" "child" (ipld/link middle)})
        recursive (fn [limit]
                    {:selector :explore-recursive
                     :limit limit
                     :sequence {:selector :explore-union
                                :members [{:selector :matcher}
                                          {:selector :explore-all
                                           :next {:selector :explore-recursive-edge}}]}})
        finite (graph/select-blocks #(get @store %) root
                                    (recursive {:mode :depth :depth 2}) limits)
        unbounded (graph/select-blocks #(get @store %) root
                                       (recursive {:mode :none}) limits)]
    (is (= #{[] ["name"] ["child"]}
           (set (map :path (:matches finite)))))
    (is (= [root middle] (mapv :cid (:blocks finite))))
    (is (= [root middle leaf] (mapv :cid (:blocks unbounded))))
    (is (some #(= ["child" "child" "name"] (:path %)) (:matches unbounded)))))

(deftest integer-path-segments-compile-to-explore-index
  (is (= {:selector :explore-fields
          :fields {"items" {:selector :explore-index :index 1
                             :next {:selector :matcher}}}}
         (graph/path-selector ["items" 1]))))

(defn drain-cursor [cursor get-fn]
  (loop [cursor cursor blocks []]
    (let [advanced (graph/advance-cursor cursor get-fn 32)
          blocks (cond-> blocks (:block advanced) (conj (:block advanced)))]
      (if (:done? advanced)
        {:cursor (:cursor advanced) :blocks blocks}
        (recur (:cursor advanced) blocks)))))

(deftest selection-cursor-reads-one-verified-block-per-advance-and-resumes
  (let [{:keys [root leaf get-fn]} (fixture)
        reads (atom [])
        get-counted (fn [cid] (swap! reads conj cid) (get-fn cid))
        selector (graph/path-selector ["child" "score"])
        cursor (graph/selection-cursor root selector limits)]
    (is (empty? @reads) "cursor creation performs no storage reads")
    (let [root-step (graph/advance-cursor cursor get-counted 32)]
      (is (= root (get-in root-step [:block :cid])))
      (is (= [root] @reads))
      (let [leaf-step (graph/advance-cursor (:cursor root-step) get-counted 32)]
        (is (= leaf (get-in leaf-step [:block :cid])))
        (is (= [root leaf] @reads))
        (let [completed (graph/advance-cursor (:cursor leaf-step) get-counted 32)
              result (graph/cursor-result (:cursor completed))]
          (is (:done? completed))
          (is (= 1.1 (-> result :matches first :value)))
          (is (= {:blocks 2
                  :bytes (+ (byte-length (get-fn root))
                            (byte-length (get-fn leaf)))
                  :matches 1}
                 (:stats result))))))))

(deftest cursor-work-budget-yields-without-reading-and-preserves-semantics
  (let [store (atom {})
        put! (fn [cid bytes] (swap! store assoc cid bytes))
        leaf (ipld/put-node! put! {"name" "leaf"})
        middle (ipld/put-node! put! {"name" "middle" "child" (ipld/link leaf)})
        root (ipld/put-node! put! {"name" "root" "child" (ipld/link middle)})
        selector {:selector :explore-recursive
                  :limit {:mode :none}
                  :sequence {:selector :explore-union
                             :members [{:selector :matcher}
                                       {:selector :explore-all
                                        :next {:selector :explore-recursive-edge}}]}}
        eager (graph/select-blocks #(get @store %) root selector limits)
        first-read (graph/advance-cursor
                    (graph/selection-cursor root selector limits)
                    #(get @store %) 1)
        yielded (graph/advance-cursor (:cursor first-read)
                                      #(get @store %) 1)
        drained (drain-cursor (:cursor yielded) #(get @store %))]
    (is (:yielded? yielded))
    (is (nil? (:block yielded)))
    (is (= [middle leaf] (mapv :cid (:blocks drained))))
    (is (= (:matches eager)
           (:matches (graph/cursor-result (:cursor drained)))))))

(deftest selection-cursor-checkpoint-round-trips-mid-traversal
  (let [{:keys [root leaf get-fn]} (fixture)
        selector (graph/path-selector ["child" "score"])
        first-step (graph/advance-cursor
                    (graph/selection-cursor root selector limits) get-fn 32)
        bytes (graph/checkpoint-cursor (:cursor first-step))
        restored (graph/restore-cursor bytes)
        drained (drain-cursor restored get-fn)]
    (is (= root (get-in first-step [:block :cid])))
    (is (= [leaf] (mapv :cid (:blocks drained))))
    (is (= 1.1 (-> drained :cursor graph/cursor-result :matches first :value)))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (graph/restore-cursor (ipld/encode {"version" 999}))))))
