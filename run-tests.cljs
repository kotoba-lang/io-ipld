(ns run-tests
  "The `ipld.link` suite on nbb.

   The repo's main ClojureScript suite runs under shadow-cljs, which compiles
   deftype fields to real properties — so it cannot fail on the one thing this
   file exists to check. nbb does not, and that difference silently broke every
   consumer that encoded a node containing a link.

   Run it with the two sibling checkouts on the classpath (base32 and cbor are
   all the link layer needs; `@noble/hashes` is only for `ipld.core`):

     nbb --classpath 'src:test:../org-ietf-cbor/src:../io-multiformats/src' run-tests.cljs"
  (:require [cljs.test :as t]
            [ipld.link-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (println (str "\nnbb: " (:test m) " tests, " (:pass m) " passed, "
                (:fail m) " failed, " (:error m) " errors"))
  (when-not (t/successful? m)
    (set! (.-exitCode js/process) 1)))

(t/run-tests 'ipld.link-test)
