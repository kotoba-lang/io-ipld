(ns ipld.link
  "Lightweight explicit IPLD Link value and DAG-CBOR tag-42 adapter.

  Parsing or re-emitting an existing CID uses dependency-free base32 only. It
  does not load CID hashing, so canonical value consumers without storage or
  content-addressing authority need no npm hash implementation."
  (:require [cbor.core :as cbor]
            [clojure.string :as str]
            [multiformats.base32 :as base32]))

(deftype Link [cid]
  #?@(:clj [Object
            (equals [_ other]
              (and (instance? Link other) (= cid (.-cid ^Link other))))
            (hashCode [_] (hash cid))
            (toString [_] (str "#ipld/link \"" cid "\""))]
      :cljs [IEquiv
             (-equiv [_ other]
               (and (instance? Link other) (= cid (.-cid ^Link other))))
             IHash
             (-hash [_] (hash cid))
             Object
             (toString [_] (str "#ipld/link \"" cid "\""))]))

(defn link
  "Wrap a base32 `b`-multibase CIDv1 string as an IPLD link."
  [cid]
  (when-not (and (string? cid) (str/starts-with? cid "b"))
    (throw (ex-info "ipld: link expects a base32 'b' multibase CID string"
                    {:cid cid})))
  (Link. cid))

(defn link? [x] (instance? Link x))

(defn link-cid [^Link value]
  (.-cid value))

(defn- link->tag-bytes [cid]
  (let [body (base32/decode (subs cid 1))]
    #?(:clj (byte-array (cons (byte 0) (seq body)))
       :cljs (js/Uint8Array. (clj->js (cons 0 (seq body)))))))

(defn- tag-bytes->link [bytes]
  (let [unsigned (map #(bit-and (int %) 0xff) (seq bytes))]
    (when-not (= 0 (first unsigned))
      (throw (ex-info
              "ipld: tag-42 byte string must start with the 0x00 identity multibase prefix"
              {:first-byte (first unsigned)})))
    (Link. (str "b" (base32/encode (rest unsigned))))))

(defn link->tag
  "A Link as tag 42 wrapping `0x00 ++ <binary CID>`."
  [value]
  (when-not (link? value)
    (throw (ex-info "ipld: link->tag expects a Link" {:value value})))
  (cbor/tagged 42 (link->tag-bytes (link-cid value))))

(defn tag->link
  "Inverse of `link->tag`; reject every tag other than 42."
  [tagged]
  (when-not (and (cbor/tagged? tagged) (= 42 (cbor/tag-number tagged)))
    (throw (ex-info "ipld: tag->link expects cbor tag 42"
                    {:tag (when (cbor/tagged? tagged)
                            (cbor/tag-number tagged))})))
  (tag-bytes->link (cbor/tag-value tagged)))
