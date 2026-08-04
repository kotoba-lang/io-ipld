(ns kotoba.value.codec
  "Stable language-facing API for the canonical `kotoba.value.v1` codec.

  The wire implementation remains in `ipld.value` because this repository also
  owns its DAG-CBOR building blocks. Consumers at compiler, provider, actor, and
  I/O boundaries should require this namespace instead: the language contract
  is a canonical value codec, not an IPLD node API."
  (:require [ipld.value :as value]))

(def codec-id value/codec-id)
(def max-safe-integer value/max-safe-integer)
(def min-safe-integer value/min-safe-integer)

(def wire-contract
  "Stable identity for bounded actor/provider/I/O value boundaries.

  The ability descriptor remains the authority for the concrete byte limit;
  this codec only enforces the limit it is given."
  {:format :kotoba.value-boundary/v1
   :codec codec-id
   :representation :bytes
   :limit-authority :ability-max-bytes})

(defn- reject! [problem data]
  (throw (ex-info (str "kotoba.value.codec: " (name problem))
                  (assoc data :problem problem :codec codec-id))))

(defn byte-count
  "Return the size of a canonical byte payload; reject non-byte inputs."
  [bytes]
  #?(:clj (if (bytes? bytes)
            (alength ^bytes bytes)
            (reject! :value/not-bytes {:value-type (type bytes)}))
     :cljs (if (or (instance? js/Uint8Array bytes)
                   (instance? js/Int8Array bytes))
             (.-length bytes)
             (reject! :value/not-bytes {:value-type (type bytes)}))))

(defn- checked-limit [max-bytes]
  (when-not (pos-int? max-bytes)
    (reject! :value/max-bytes-invalid {:max-bytes max-bytes}))
  max-bytes)

(defn float64 [number] (value/float64 number))
(defn float64? [x] (value/float64? x))
(defn float64-value [x] (value/float64-value x))

(defn value->form
  "Return the canonical tagged form embedded by larger org-owned codecs."
  [x]
  (value/value->form x))

(defn form->value
  "Validate and decode one canonical tagged value form."
  [form]
  (value/form->value form))

(defn encode-value
  "Encode one admitted value as canonical `kotoba.value.v1` bytes."
  [x]
  (value/encode-value x))

(defn decode-value
  "Decode canonical `kotoba.value.v1` bytes and reject noncanonical input."
  [bytes]
  (value/decode-value bytes))

(defn encode-bounded
  "Encode VALUE and reject an envelope larger than the ability MAX-BYTES."
  [value max-bytes]
  (let [limit (checked-limit max-bytes)
        encoded (encode-value value)
        actual (byte-count encoded)]
    (when (> actual limit)
      (reject! :value/max-bytes-exceeded
               {:direction :encode :bytes actual :max-bytes limit}))
    encoded))

(defn decode-bounded
  "Enforce the ability MAX-BYTES before decoding canonical BYTES."
  [bytes max-bytes]
  (let [limit (checked-limit max-bytes)
        actual (byte-count bytes)]
    (when (> actual limit)
      (reject! :value/max-bytes-exceeded
               {:direction :decode :bytes actual :max-bytes limit}))
    (decode-value bytes)))
