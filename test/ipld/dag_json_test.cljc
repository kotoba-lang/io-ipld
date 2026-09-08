(ns ipld.dag-json-test
  "Pinned to bytes this repository did not produce.

  The advertisement below was fetched 2026-08-26 from a live third-party
  IPNI publisher (http://157.148.101.187:3105). Its served bytes hash to
  the CID it is served under, which makes those bytes the canonical form
  by definition -- so an encoder that claims to write DAG-JSON has to
  reproduce them exactly, and any disagreement is this encoder being
  wrong rather than a matter of taste.

  What that pins, which a self-consistent test could not: that map keys
  are sorted bytewise rather than kept in schema order, that byte strings
  are standard base64 without padding rather than base64url, and that
  `<`, `>` and `&` are not escaped the way Go's encoding/json escapes
  them by default."
  (:require [kotoba.lang.text :as str]
            [clojure.test :refer [deftest is testing]]
            [ipld.dag-json :as dj]
            [ipld.link :as link]))

(def served
  (str "{\"Addresses\":[\"/ip4/157.148.101.187/tcp/58419\"],\"ContextID\":{\"/\":{\"bytes\":\"\"}},"
       "\"Entries\":{\"/\":\"bafkreehdwdcefgh4dqkjv67uzcmw7oje\"},"
       "\"ExtendedProvider\":{\"Override\":false,\"Providers\":[{\"Addresses\":"
       "[\"/ip4/157.148.101.187/tcp/58421/http\"],"
       "\"ID\":\"12D3KooWMipNxukQPsg76mfxKK7XEcchThK3n974z7tbbyYBY9tP\","
       "\"Metadata\":{\"/\":{\"bytes\":\"oBIA\"}},\"Signature\":{\"/\":{\"bytes\":\""
       "CiQIARIgsOBlEOpFFCW9UgopYSHO3PJp8fkAaCQNitcRihLc45wSKS9pbmRleGVyL2luZ2VzdC9leHRlbmRlZFByb3ZpZGVyU2lnbmF0dXJlGiISIODBwY7BftZidO2WXPj8KJ0vWv5ZVKGiYOnZDtlgkkr5KkBCkSYSj3euxY1Y2H1amM2NONtcEY7j9uwIAxJ9aMPQT5H0Ta0T+01qb70y1B+JxL6gWLEPRbd6f7PjHrFG1ZsO"
       "\"}}}]},\"IsRm\":false,\"Metadata\":{\"/\":{\"bytes\":\"\"}},"
       "\"PreviousID\":{\"/\":\"baguqeeralczfloeijolao67txipdx33w6zldoiuj54ugmuruqky5a6astsoq\"},"
       "\"Provider\":\"12D3KooWMipNxukQPsg76mfxKK7XEcchThK3n974z7tbbyYBY9tP\","
       "\"Signature\":{\"/\":{\"bytes\":\""
       "CiQIARIgsOBlEOpFFCW9UgopYSHO3PJp8fkAaCQNitcRihLc45wSGy9pbmRleGVyL2luZ2VzdC9hZFNpZ25hdHVyZRoiEiDOWjb9tDDJMWkUzAfAV+ZifDJGxzNpW4Stum89E7ux/SpAlRUTglbOClohZcG1LkHLNetjec9sXJ9lOeEWbQlds0Laj31GUNXMxkWo8L+QuOXTqK4aHy/5Z1vJFoGSzINfDg"
       "\"}}}"))

(def served-cid "baguqeerau3bc6i65bol7yn73jr3ix3xgfl57w635bih6szvpon2b27ajf47q")

(defn- b64
  "Decoded by the HOST, not by the code under test -- otherwise the
  fixture would be built from the thing it is meant to check."
  [s]
  #?(:clj (.decode (java.util.Base64/getDecoder) ^String s)
     :cljs (js/Uint8Array.from (js/Buffer.from s "base64"))))

(def ep-signature
  (b64 (str "CiQIARIgsOBlEOpFFCW9UgopYSHO3PJp8fkAaCQNitcRihLc45wSKS9pbmRleGVyL2luZ2VzdC9leHRlbmRlZFByb3ZpZGVyU2lnbmF0dXJl"
            "GiISIODBwY7BftZidO2WXPj8KJ0vWv5ZVKGiYOnZDtlgkkr5KkBCkSYSj3euxY1Y2H1amM2NONtcEY7j9uwIAxJ9aMPQT5H0Ta0T+01qb70y1B+JxL6gWLEPRbd6f7PjHrFG1ZsO")))

(def peer "12D3KooWMipNxukQPsg76mfxKK7XEcchThK3n974z7tbbyYBY9tP")

(defn advertisement
  "Field order here is the SCHEMA order go-libipni declares, deliberately
  not alphabetical. If the encoder kept insertion order the output would
  differ from the served bytes, and this test would say so."
  []
  (array-map
   "PreviousID" (link/link "baguqeeralczfloeijolao67txipdx33w6zldoiuj54ugmuruqky5a6astsoq")
   "Provider" peer
   "Addresses" ["/ip4/157.148.101.187/tcp/58419"]
   "Signature" (b64 "CiQIARIgsOBlEOpFFCW9UgopYSHO3PJp8fkAaCQNitcRihLc45wSGy9pbmRleGVyL2luZ2VzdC9hZFNpZ25hdHVyZRoiEiDOWjb9tDDJMWkUzAfAV+ZifDJGxzNpW4Stum89E7ux/SpAlRUTglbOClohZcG1LkHLNetjec9sXJ9lOeEWbQlds0Laj31GUNXMxkWo8L+QuOXTqK4aHy/5Z1vJFoGSzINfDg")
   "Entries" (link/link "bafkreehdwdcefgh4dqkjv67uzcmw7oje")
   "ContextID" (b64 "")
   "Metadata" (b64 "")
   "IsRm" false
   "ExtendedProvider"
   (array-map
    "Providers" [(array-map
                  "ID" peer
                  "Addresses" ["/ip4/157.148.101.187/tcp/58421/http"]
                  "Metadata" (b64 "oBIA")
                  "Signature" ep-signature)]
    "Override" false)))

(deftest reproduces-a-real-advertisement-byte-for-byte
  (testing "the canonical form is what a live publisher serves"
    (is (= served (dj/encode-string (advertisement))))))

(deftest addresses-the-same-block
  (testing "and those bytes carry the CID it is served under"
    (is (= served-cid (dj/cid (dj/encode (advertisement)))))
    (is (= served-cid (:cid (dj/node->block (advertisement)))))))

(deftest keys-are-sorted-not-kept
  (testing "insertion order must not survive"
    (is (= "{\"a\":1,\"b\":2,\"c\":3}"
           (dj/encode-string (array-map "c" 3 "a" 1 "b" 2))))
    (is (= (dj/encode-string (array-map "b" 1 "a" 2))
           (dj/encode-string (array-map "a" 2 "b" 1))))))

(deftest bytes-are-standard-base64-unpadded
  (testing "not base64url, and not padded"
    ;; 0xFB 0xFF encodes to `+/` in the standard alphabet and `-_` in url-safe.
    (is (str/includes? (dj/encode-string (b64 "+/8=")) "\"+/8\""))
    (is (not (str/includes? (dj/encode-string (b64 "oBIA")) "=")))))

(deftest json-only-escapes-what-json-requires
  (testing "Go's encoding/json escapes < > & by default; DAG-JSON does not"
    (is (= "\"<a>&b\"" (dj/encode-string "<a>&b")))
    (is (= "\"say \\\"hi\\\"\"" (dj/encode-string "say \"hi\"")))
    (is (= "\"a\\nb\"" (dj/encode-string "a\nb")))))

(deftest one-changed-byte-is-a-different-block
  (testing "the CID assertion discriminates -- it is not comparing a constant"
    (let [changed (assoc (advertisement) "IsRm" true)]
      (is (not= served (dj/encode-string changed)))
      (is (not= served-cid (dj/cid (dj/encode changed)))))))

(deftest a-link-is-not-a-string
  (testing "a CID written as a plain string addresses a different block"
    (is (= "{\"/\":\"bafkqaaa\"}" (dj/encode-string (link/link "bafkqaaa"))))
    (is (not= (dj/encode-string (link/link "bafkqaaa"))
              (dj/encode-string "bafkqaaa")))))
