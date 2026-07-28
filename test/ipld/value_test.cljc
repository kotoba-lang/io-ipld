(ns ipld.value-test
  "VC1 conformance for `kotoba.value.v1`.

  The hex table below is the cross-runtime contract, not a convenience: this
  file is `.cljc` and CI runs it BOTH on the JVM (`clojure -M:test`) and as
  real ClojureScript on node (`npm run test:cljs`), so a byte that differs
  between runtimes fails here rather than surfacing later as two CIDs for one
  value. `kotoba-lang/compiler:src/kotoba/compiler/artifact.cljc` records the
  live incident this guards against — the same KIR hashing differently on JVM
  and nbb because `pr-str` rendered a bigint differently."
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [ipld.core :as ipld]
            [ipld.value :as v]
            [multiformats.core :as mf]
            [cbor.core :as cbor]))

(defrecord UnadmittedRecord [x])

(defn- hx [b]
  (apply str (map (fn [x]
                    #?(:clj (format "%02x" (bit-and (int x) 0xff))
                       :cljs (let [h (.toString (bit-and x 0xff) 16)]
                               (if (= 1 (count h)) (str "0" h) h))))
                  (seq b))))

(defn- bs [& xs]
  #?(:clj (byte-array (map unchecked-byte xs))
     :cljs (js/Uint8Array. (into-array xs))))

(defn- rejects? [f]
  (try (f) false
       (catch #?(:clj Exception :cljs :default) e
         (boolean (:problem (ex-data e))))))

(defn- problem-of [f]
  (try (f) nil
       (catch #?(:clj Exception :cljs :default) e (:problem (ex-data e)))))

(def some-cid (mf/kotoba-cid "ibuki"))

;; ── exact wire form ──────────────────────────────────────────────────────────
;; 82   array(2)   -- every value is [type-code payload]
;; xx   type code
;; …    payload
(deftest scalar-wire-form-is-exact
  (is (= "8200f6" (hx (v/encode-value nil))))
  (is (= "8201f5" (hx (v/encode-value true))))
  (is (= "8201f4" (hx (v/encode-value false))))
  (is (= "820200" (hx (v/encode-value 0))))
  (is (= "82021818" (hx (v/encode-value 24))))
  (is (= "820220" (hx (v/encode-value -1))))
  (is (= "82046161" (hx (v/encode-value "a"))) "string  -> [4, text]")
  (is (= "82056161" (hx (v/encode-value :a))) "keyword -> [5, text]")
  (is (= "82066161" (hx (v/encode-value 'a))) "symbol  -> [6, text]")
  (is (= "82074101" (hx (v/encode-value (bs 1)))) "bytes   -> [7, byte-string]")
  (is (= "820744deadbeef" (hx (v/encode-value (bs 0xde 0xad 0xbe 0xef)))))
  (testing "the same text under three different codes is three different values"
    (is (= 3 (count (set (map hx [(v/encode-value "a")
                                  (v/encode-value :a)
                                  (v/encode-value 'a)])))))))

(deftest namespaced-names-carry-their-namespace
  (is (= "82056a6b6f746f62612f6f6e65" (hx (v/encode-value :kotoba/one))))
  (is (= :kotoba/one (v/decode-value (v/encode-value :kotoba/one))))
  (is (= "kotoba" (namespace (v/decode-value (v/encode-value :kotoba/one)))))
  (is (= 'kotoba/one (v/decode-value (v/encode-value 'kotoba/one)))))

(deftest link-is-a-real-tag-42
  ;; 8208 d82a 5825 00 …  -- [8, tag(42, h'00 ++ binary CID')]
  (let [l (ipld/link some-cid)]
    (is (= (str "8208d82a582500" (hx (mf/cid->bytes some-cid)))
           (hx (v/encode-value l))))
    (is (= l (v/decode-value (v/encode-value l))))
    (testing "a link inside a value is still a tag-42 link to generic IPLD tooling"
      (is (= 42 (cbor/tag-number (second (cbor/decode (v/encode-value l)))))))))

(deftest floats-round-trip-through-the-explicit-wrapper
  ;; 8203 48 <8 bytes big-endian IEEE-754 binary64>
  (is (= "8203480000000000000000" (hx (v/encode-value (v/float64 0.0)))))
  (is (= "8203483ff0000000000000" (hx (v/encode-value (v/float64 1.0)))))
  (is (= "820348bff0000000000000" (hx (v/encode-value (v/float64 -1.0)))))
  (is (= 0.5 (v/float64-value (v/decode-value (v/encode-value (v/float64 0.5))))))
  (is (= -273.15 (v/float64-value (v/decode-value (v/encode-value (v/float64 -273.15)))))))

;; ── the reason the wrapper exists ────────────────────────────────────────────
(deftest a-bare-non-integral-number-is-rejected-not-guessed
  ;; On cljs `(integer? 1.0)` is true and on the JVM it is false, so classifying
  ;; a bare number by runtime type would give one source program two encodings.
  ;; 1.5 is non-integral on BOTH runtimes, so this assertion is meaningful on
  ;; both: reject, never silently coerce.
  (is (= :value/unwrapped-number (problem-of #(v/encode-value 1.5))))
  (is (= "820201" (hx (v/encode-value 1)))
      "an integral value has one encoding on every runtime"))

(deftest non-finite-floats-are-rejected-at-construction-and-on-the-wire
  (is (= :value/float-nan (problem-of #(v/float64 (/ 0.0 0.0)))))
  (is (= :value/float-infinite (problem-of #(v/float64 ##Inf))))
  (is (= :value/float-infinite (problem-of #(v/float64 ##-Inf))))
  (testing "-0.0 is rejected, NOT normalized to 0.0 — normalizing moves an identity"
    (is (= :value/float-negative-zero (problem-of #(v/float64 -0.0)))))
  (testing "a peer cannot smuggle one back in through decode"
    (let [neg-inf-bits (cbor/encode [3 (bs 0xff 0xf0 0 0 0 0 0 0)])]
      (is (= :value/float-infinite (problem-of #(v/decode-value neg-inf-bits)))))))

(deftest integers-outside-the-safe-range-are-rejected-not-truncated
  ;; cbor.core's own `byte-at` docstring: the cljs path is exact only to 2^53.
  (is (= 9007199254740991 (v/decode-value (v/encode-value 9007199254740991))))
  (is (= :value/integer-out-of-range (problem-of #(v/encode-value 9007199254740992))))
  (is (= :value/integer-out-of-range (problem-of #(v/encode-value -9007199254740992)))))

;; ── collections ──────────────────────────────────────────────────────────────
(deftest ordered-collections-keep-their-order-and-stay-distinct
  (is (= "821082820201820202" (hx (v/encode-value [1 2]))))
  (is (= "821182820201820202" (hx (v/encode-value '(1 2)))))
  (is (= [1 2] (v/decode-value (v/encode-value [1 2]))))
  (is (= '(1 2) (v/decode-value (v/encode-value '(1 2)))))
  (testing "vector and list are different values, not one 'sequential'"
    (is (not= (hx (v/encode-value [1 2])) (hx (v/encode-value '(1 2)))))
    (is (vector? (v/decode-value (v/encode-value [1 2]))))
    (is (seq? (v/decode-value (v/encode-value '(1 2)))))))

(deftest set-and-map-order-is-independent-of-insertion-order
  (let [a (v/encode-value #{3 1 2})
        b (v/encode-value #{2 3 1})]
    (is (= (hx a) (hx b)) "a set has one encoding regardless of how it was built")
    (is (= #{1 2 3} (v/decode-value a))))
  (let [a (v/encode-value {:b 2 :a 1})
        b (v/encode-value (array-map :a 1 :b 2))]
    (is (= (hx a) (hx b)) "a map has one encoding regardless of key insertion order")
    (is (= {:a 1 :b 2} (v/decode-value a)))))

(deftest ordering-uses-unsigned-bytes
  ;; The trap: a JVM byte-array compared signed sorts 0x80-0xff BEFORE 0x00,
  ;; while cljs `Uint8Array` is unsigned and sorts them after. If the sort
  ;; leaked the platform's signedness, these two runtimes would disagree here.
  (let [wire (hx (v/encode-value #{(bs 0xff) (bs 0x00)}))]
    (is (= wire (hx (v/encode-value #{(bs 0x00) (bs 0xff)}))))
    (is (< (.indexOf wire "4100") (.indexOf wire "41ff"))
        "0x00 sorts before 0xff — unsigned, on every runtime")))

(deftest nesting-round-trips
  (let [value {:name "ibuki"
               :tags #{:a :b}
               :dims [1 2 3]
               :ratio (v/float64 0.25)
               :ref (ipld/link some-cid)
               :raw (bs 0xde 0xad)
               :nested {:deep [{:k :v}]}}
        out (v/decode-value (v/encode-value value))]
    (is (= (:name value) (:name out)))
    (is (= (:tags value) (:tags out)))
    (is (= (:dims value) (:dims out)))
    (is (= 0.25 (v/float64-value (:ratio out))))
    (is (= (:ref value) (:ref out)))
    (is (= "dead" (hx (:raw out))))
    (is (= (:nested value) (:nested out)))
    (is (= (hx (v/encode-value value)) (hx (v/encode-value out)))
        "re-encoding a decoded value is byte-identical")))

(deftest keyword-keys-survive-persistence
  ;; The defect that motivates the whole codec: through the NODE codec a
  ;; keyword comes back as a string, so the value read is not the value
  ;; written even though its CID verifies.
  (is (= {"a" 1} (ipld/decode (ipld/encode {:a 1}))) "node codec: keyword key -> string")
  (is (= {:a 1} (v/decode-value (v/encode-value {:a 1}))) "value codec: keyword key stays a keyword")
  (is (set? (v/decode-value (v/encode-value #{"x" "y"})))))

;; ── fail-closed ──────────────────────────────────────────────────────────────
(deftest unadmitted-types-are-rejected-closed
  (is (= :value/unsupported-type
         (problem-of #(v/encode-value #?(:clj (java.util.Date.) :cljs (js/Date.))))))
  (is (= :value/record-unsupported (problem-of #(v/encode-value (->UnadmittedRecord 1)))))
  (testing "a raw cbor tag is not a value — links are constructed with ipld/link"
    (is (rejects? #(v/encode-value (cbor/tagged 42 (bs 0)))))))

(deftest malformed-wire-data-is-rejected-closed
  (is (= :value/form-malformed (problem-of #(v/decode-value (cbor/encode [1])))))
  (is (= :value/form-malformed (problem-of #(v/decode-value (cbor/encode "not-a-form")))))
  (is (= :value/unknown-type-code (problem-of #(v/decode-value (cbor/encode [99 "x"])))))
  (is (= :value/string-payload (problem-of #(v/decode-value (cbor/encode [4 7])))))
  (is (= :value/boolean-payload (problem-of #(v/decode-value (cbor/encode [1 "yes"])))))
  (is (= :value/nil-payload (problem-of #(v/decode-value (cbor/encode [0 "x"]))))))

(deftest non-canonical-collections-are-rejected-on-decode
  ;; A peer must not be able to hand back a canonically-ADDRESSED block whose
  ;; contents are not in canonical FORM.
  (let [ascending  (cbor/encode [18 [[2 1] [2 2]]])
        descending (cbor/encode [18 [[2 2] [2 1]]])
        duplicated (cbor/encode [18 [[2 1] [2 1]]])]
    (is (= #{1 2} (v/decode-value ascending)))
    (is (= :value/set-not-canonical (problem-of #(v/decode-value descending))))
    (is (= :value/set-not-canonical (problem-of #(v/decode-value duplicated)))))
  (let [descending (cbor/encode [19 [[[2 2] [2 20]] [[2 1] [2 10]]]])
        malformed  (cbor/encode [19 [[[2 1]]]])]
    (is (= :value/map-not-canonical (problem-of #(v/decode-value descending))))
    (is (= :value/map-entry-malformed (problem-of #(v/decode-value malformed))))))

(deftest encoding-is-a-function-of-the-value-alone
  (doseq [x [nil true false 0 -1 "" "kotoba" :a :kotoba/one 'a
             #{} [] '() {} {:a #{1 2}} [[[:deep]]] {:k [1 {:n #{:x}}]}]]
    (is (= (hx (v/encode-value x)) (hx (v/encode-value x))))
    (is (= x (v/decode-value (v/encode-value x)))
        (str "round-trip: " (pr-str x)))))
