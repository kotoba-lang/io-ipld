(ns run-tests
  "The lightweight Link and Schema suites on nbb.

   The compiled ClojureScript suite cannot expose SCI's type/protocol boundary.
   This is therefore a required, separate qualification target for consumers
   that execute the shared `.cljc` sources through nbb.

   Run it with the two sibling checkouts on the classpath (base32 and cbor are
   all the link layer needs; `@noble/hashes` is only for `ipld.core`):

     nbb --classpath \"$(clojure -Spath)\" run-tests.cljs"
  (:require [cljs.test :as t]
            [ipld.link-test]
            [ipld.schema-test]
            [kotoba.value.value-cid-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (println (str "\nnbb: " (:test m) " tests, " (:pass m) " passed, "
                (:fail m) " failed, " (:error m) " errors"))
  (when (pos? (+ (or (:fail m) 0) (or (:error m) 0)))
    (set! (.-exitCode js/process) 1)))

(t/run-tests 'ipld.link-test 'ipld.schema-test 'kotoba.value.value-cid-test)
