(ns ipld.core-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [ipld.core :as ipld]
            [multiformats.core :as mf]
            [cbor.core :as cbor]))

(defn- hx [b]
  (apply str (map (fn [x]
                    #?(:clj (format "%02x" (bit-and (int x) 0xff))
                       :cljs (let [h (.toString (bit-and x 0xff) 16)]
                               (if (= 1 (count h)) (str "0" h) h))))
                  (seq b))))

(def some-cid (mf/kotoba-cid "ibuki")) ; deterministic CIDv1 dag-cbor

;; ── exact wire form ───────────────────────────────────────────────────────────
(deftest link-encodes-as-tag-42-identity-prefixed-binary-cid
  ;; d8 2a           tag(42)
  ;; 58 25           byte string, length 37
  ;; 00              identity multibase prefix
  ;; 01 71 12 20 …   binary CIDv1: version=1, codec=dag-cbor, sha2-256, len 32
  (let [cid-hex (hx (mf/cid->bytes some-cid))]
    (is (= (str "d82a582500" cid-hex) (hx (ipld/encode (ipld/link some-cid)))))
    (is (= "01711220" (subs cid-hex 0 8)))))

(deftest non-link-data-encodes-exactly-like-dag-cbor
  ;; without links, ipld/encode == cbor/encode byte for byte
  (let [node {"a" 1 "b" [2 3] "s" "x"}]
    (is (= (hx (cbor/encode node)) (hx (ipld/encode node))))))

;; ── round-trip ────────────────────────────────────────────────────────────────
(deftest link-roundtrip
  (let [node {"name" "root"
              "children" [{"k" "a" "cid" (ipld/link some-cid)}
                          {"k" "b" "cid" (ipld/link some-cid)}]
              "prev" nil}
        decoded (ipld/decode (ipld/encode node))]
    (is (= "root" (get decoded "name")))
    (is (nil? (get decoded "prev")))
    (let [l (get-in decoded ["children" 0 "cid"])]
      (is (ipld/link? l))
      (is (= some-cid (ipld/link-cid l))))))

(deftest cid-is-stable-and-content-addressed
  (let [n1 {"x" (ipld/link some-cid) "n" 1}
        n2 {"n" 1 "x" (ipld/link some-cid)}          ; same map, different literal order
        b1 (ipld/encode n1) b2 (ipld/encode n2)]
    (is (= (hx b1) (hx b2)))                          ; canonical key sort
    (is (= (ipld/cid b1) (ipld/cid b2)))
    (is (not= (ipld/cid b1) (ipld/cid (ipld/encode {"n" 2}))))))

;; ── spec guards ───────────────────────────────────────────────────────────────
(deftest only-tag-42-is-accepted
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (ipld/decode (cbor/encode (cbor/tagged 43 1)))))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (ipld/encode {"x" (cbor/tagged 42 "raw")}))) ; raw tags rejected too
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (ipld/link "zNotBase32")))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (ipld/encode {1 "non-string-key"}))))

;; ── storage ports + generic walk ─────────────────────────────────────────────
(deftest put-get-links-walk
  (let [store (atom {})
        put!  (fn [cid bytes] (swap! store assoc cid bytes))
        get-fn (fn [cid] (get @store cid))
        leaf-cid (ipld/put-node! put! {"kind" "leaf" "v" 1})
        root-cid (ipld/put-node! put! {"kind" "internal"
                                       "children" [["a" (ipld/link leaf-cid)]
                                                   ["b" (ipld/link leaf-cid)]]})
        root (ipld/get-node get-fn root-cid)]
    (is (= [leaf-cid leaf-cid] (ipld/links root)))
    (is (= [] (ipld/links (ipld/get-node get-fn leaf-cid))))
    (is (nil? (ipld/get-node get-fn some-cid)))       ; absent block -> nil
    (is (= root-cid (ipld/cid (get @store root-cid))))))

(deftest link-equality
  (is (= (ipld/link some-cid) (ipld/link some-cid)))
  (is (not= (ipld/link some-cid) (ipld/link (mf/kotoba-cid "other"))))
  (is (= (hash (ipld/link some-cid)) (hash (ipld/link some-cid)))))
