(ns ipld.dag-json
  "DAG-JSON: the IPLD codec that reads as JSON and still addresses.

  Written because IPNI publishes advertisements in it and nothing here
  spoke it. The shape is not guessed -- it is pinned to an advertisement
  fetched from a live third-party publisher, whose served bytes hash to
  the CID it is served under. That makes those bytes the canonical form
  by definition, and this encoder has to reproduce them exactly.

  Three rules carry the whole codec:

    map keys        sorted bytewise ascending, not schema order
    a link          {\"/\": \"bafy…\"}
    a byte string   {\"/\": {\"bytes\": \"<base64, standard alphabet, unpadded>\"}}

  and no insignificant whitespace anywhere.

  Like `ipld.core/decode`, decoding re-encodes and refuses bytes that are
  not canonical. A codec whose job is identity cannot let two byte
  strings denote one value."
  (:require [clojure.string :as str]
            [ipld.data-model :as data-model]
            [ipld.link :as link]
            [multiformats.core :as mf]))

(def ^:const codec
  "multicodec `dag-json`."
  0x0129)

;; ── base64, standard alphabet, no padding ───────────────────────────────────
;; Not base64url: the fixture's signature carries `+` and `/` unescaped.

(def ^:private b64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn- byte-seq [b]
  #?(:clj (map #(bit-and % 0xff) (seq b))
     :cljs (map #(bit-and % 0xff) (array-seq b))))

(defn base64-encode [bytes]
  (let [bs (vec (byte-seq bytes))]
    (loop [i 0 out (transient [])]
      (if (>= i (count bs))
        (apply str (persistent! out))
        (let [b0 (nth bs i)
              b1 (nth bs (+ i 1) nil)
              b2 (nth bs (+ i 2) nil)
              n (bit-or (bit-shift-left b0 16)
                        (bit-shift-left (or b1 0) 8)
                        (or b2 0))
              c (fn [shift] (nth b64-alphabet (bit-and (bit-shift-right n shift) 0x3F)))]
          (recur (+ i 3)
                 (cond-> (-> out (conj! (c 18)) (conj! (c 12)))
                   b1 (conj! (c 6))
                   b2 (conj! (c 0)))))))))

(defn base64-decode [s]
  (let [idx (fn [ch] (str/index-of b64-alphabet (str ch)))]
    (loop [chars (seq s) bits 0 value 0 out (transient [])]
      (if-let [ch (first chars)]
        (if-let [i (idx ch)]
          (let [value (bit-or (bit-shift-left value 6) i)
                bits (+ bits 6)]
            (if (>= bits 8)
              (recur (rest chars) (- bits 8) value
                     (conj! out (bit-and (bit-shift-right value (- bits 8)) 0xFF)))
              (recur (rest chars) bits value out)))
          (throw (ex-info "dag-json: not base64" {:char ch})))
        (persistent! out)))))

;; ── JSON string escaping ────────────────────────────────────────────────────
;; Only what JSON requires. `<`, `>` and `&` are NOT escaped: Go's
;; encoding/json escapes them by default, go-ipld-prime's dagjson does not,
;; and escaping them here would produce bytes with a different CID.

(defn- char-code
  "ClojureScript has no character type: `(seq \"ab\")` yields one-character
  STRINGS, and `(int \"a\")` is 0. Reading a code point through `int` made
  every character escape as \\u0000 on cljs while the JVM was green -- the
  encoder and its byte-for-byte fixture agreed on one runtime and not the
  other, which is why the cljs runner is not optional."
  [ch]
  #?(:clj (int ^char ch)
     :cljs (.charCodeAt (str ch) 0)))

(defn- unicode-escape [n]
  (let [hex #?(:clj (Integer/toHexString n) :cljs (.toString n 16))
        padded (str "000" hex)]
    (str "\\u" (subs padded (- (count padded) 4)))))

(defn- escape [s]
  (apply str
         (map (fn [ch]
                (let [n (char-code ch)]
                  (cond
                    (= (str ch) "\"") "\\\""
                    (= (str ch) "\\") "\\\\"
                    (= n 8) "\\b"
                    (= n 9) "\\t"
                    (= n 10) "\\n"
                    (= n 12) "\\f"
                    (= n 13) "\\r"
                    (< n 0x20) (unicode-escape n)
                    :else (str ch))))
              s)))

(defn- key-string [k]
  (cond (string? k) k
        (keyword? k) (name k)
        :else (throw (ex-info "dag-json: map keys must be strings" {:key k}))))

(defn- write [node]
  (let [kind (data-model/kind node)]
    (case kind
      :null "null"
      :bool (if node "true" "false")
      :int (str node)
      :float (throw (ex-info "dag-json: floats are not written by this encoder"
                             {:value node}))
      :string (str "\"" (escape node) "\"")
      :bytes (str "{\"/\":{\"bytes\":\"" (base64-encode node) "\"}}")
      :link (str "{\"/\":\"" (link/link-cid node) "\"}")
      :list (str "[" (str/join "," (map write node)) "]")
      :map (str "{"
                (str/join ","
                          (->> (map (fn [[k v]] [(key-string k) v]) node)
                               (sort-by first)
                               (map (fn [[k v]] (str "\"" (escape k) "\":" (write v))))))
                "}")
      (throw (ex-info "dag-json: not Data Model data" {:kind kind :value node})))))

(defn encode-string
  "Canonical DAG-JSON text for `node`."
  [node]
  (data-model/validate! node)
  (write node))

(defn encode
  "Canonical DAG-JSON bytes for `node`."
  [node]
  (let [s (encode-string node)]
    #?(:clj (.getBytes ^String s "UTF-8")
       :cljs (.encode (js/TextEncoder.) s))))

(defn cid
  "CIDv1 dag-json sha2-256 of already-encoded block bytes."
  [bytes]
  (mf/cidv1 codec (mf/multihash-sha256 bytes)))

(defn node->block
  "Encode `node` and address it: `{:cid <string> :bytes <bytes>}`."
  [node]
  (let [bytes (encode node)]
    {:cid (cid bytes) :bytes bytes}))
