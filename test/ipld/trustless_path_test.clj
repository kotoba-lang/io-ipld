(ns ipld.trustless-path-test
  "P2-1 of ADR-2608170400: the trustless path vectors.

  A trustless path is a claim -- \"this value is at this path under this root\"
  -- plus the blocks needed to check it. Each vector below is one way that
  claim can be wrong, and the answer that separates it from the others."
  (:require [clojure.test :refer [deftest is testing]]
            [ipld.core :as ipld]
            [ipld.graph :as graph]
            [ipld.link :as link]
            [multiformats.core :as mf]))

(defn- world []
  (let [blocks (atom {})
        put! (fn [node]
               (let [{:keys [cid bytes]} (ipld/node->block node)]
                 (swap! blocks assoc cid bytes)
                 cid))
        shared (put! {"s" 1})
        a (put! {"x" (link/link shared)})
        b (put! {"y" (link/link shared)})
        root (put! {"a" (link/link a) "b" (link/link b)})]
    {:blocks blocks :shared shared :a a :b b :root root
     :store (fn [cid] (get @blocks cid))}))

(def ^:private limits
  {:max-blocks 32 :max-bytes 8192 :max-depth 16 :max-matches 16})

(defn- failure [f]
  (try (do (f) nil) (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest a-path-returns-its-value-and-the-blocks-that-prove-it
  (let [{:keys [store root]} (world)
        result (graph/resolve-path store root ["a" "x" "s"] limits)]
    (is (= 1 (:value result)))
    (is (= ["a" "x" "s"] (:path result)))
    (testing "root-first, and exactly the blocks the path crosses"
      (is (= 3 (count (:blocks result))))
      (is (= root (:cid (first (:blocks result))))))))

(deftest a-block-reached-twice-is-proved-once
  ;; Both branches link the same leaf. A proof that shipped it twice would be
  ;; correct and wasteful; one that shipped it zero times would be incorrect.
  (let [{:keys [store root]} (world)
        result (graph/select-blocks
                store root
                {:selector :explore-all :next {:selector :explore-all :next {:selector :matcher}}}
                limits)
        cids (map :cid (:blocks result))]
    (is (= (count cids) (count (set cids))))
    (is (contains? (set cids) root))))

(deftest the-ways-a-path-can-fail-are-told-apart
  (let [{:keys [blocks store root a b]} (world)]
    (testing "a block the path needs is absent"
      (is (= :ipld/missing-block
             (failure #(graph/resolve-path
                        (fn [cid] (when (not= cid a) (store cid)))
                        root ["a" "x" "s"] limits)))))
    (testing "the store answers with bytes that are not that CID"
      ;; The store is the untrusted party here. This is the check that makes
      ;; the path trustless rather than merely convenient.
      (is (= :ipld/cid-mismatch
             (failure #(graph/resolve-path
                        (fn [cid] (if (= cid a) (store b) (store cid)))
                        root ["a" "x" "s"] limits)))))
    (testing "the link points at a codec this traversal cannot decode"
      ;; Distinct from a mismatch, and it has to be: the store is telling the
      ;; truth. Reporting a mismatch here sends the reader hunting corruption
      ;; that is not there.
      (let [payload (ipld/encode {"z" 9})
            raw-cid (mf/cidv1-raw payload)
            _ (swap! blocks assoc raw-cid payload)
            {:keys [cid bytes]} (ipld/node->block {"w" (link/link raw-cid)})
            _ (swap! blocks assoc cid bytes)]
        (is (= :ipld/unsupported-codec
               (failure #(graph/resolve-path store cid ["w" "z"] limits))))))))

(deftest a-cid-declares-its-codec
  (is (= mf/codec-dag-cbor (ipld/cid-codec (ipld/cid (ipld/encode {"a" 1})))))
  (is (= mf/codec-raw (ipld/cid-codec (mf/cidv1-raw (ipld/encode {"a" 1})))))
  (is (nil? (ipld/cid-codec "not-a-cid"))))
