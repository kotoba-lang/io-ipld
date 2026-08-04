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
