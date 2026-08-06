(ns ipld.link-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [ipld.link :as link]))

(def cid
  "bafyreibwzifccnbxlg3p7yh4pwqbnhi7z3q6z5a4x3v6xj4v2q6f6gq2vq")

(deftest lightweight-link-tag-round-trip
  (let [value (link/link cid)]
    (is (link/link? value))
    (is (= cid (link/link-cid value)))
    (is (= value (link/tag->link (link/link->tag value))))))

;; ---------------------------------------------------------------------------
;; Portability: no deftype field access
;; ---------------------------------------------------------------------------
;;
;; These pass trivially on the JVM and under shadow-cljs, which compiles
;; deftype fields to real properties. They fail under nbb when `link-cid` or
;; either equality implementation reaches for `.-cid`, and nbb is where this
;; repo's consumers actually broke: `link-cid` returned nil, so `link->tag`
;; threw on `(subs nil 1)` and no node containing a link could be encoded.
;;
;; `run-tests.cljs` exists so this is checked on that runtime rather than
;; assumed from the one where it cannot fail.

(deftest link-cid-does-not-depend-on-field-access
  (is (string? (link/link-cid (link/link cid))))
  (is (= cid (link/link-cid (link/link cid)))))

(deftest link-equality-contract-holds
  (let [a (link/link cid) b (link/link cid)
        other (link/link "bafyreibwzifccnbxlg3p7yh4pwqbnhi7z3q6z5a4x3v6xj4v2q6f6gq2vr")]
    (is (= a b) "same CID means equal")
    (is (= (hash a) (hash b)))
    (is (not= a other))
    (is (= 1 (count (into #{} [a b]))) "a set of equal links must collapse")
    (is (= :found (get {a :found} b)) "and a map keyed by link must be lookupable")))

(deftest encoding-a-node-with-a-link-works
  (is (some? (link/link->tag (link/link cid))))
  (is (= (link/link cid) (link/tag->link (link/link->tag (link/link cid))))))
