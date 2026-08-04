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
