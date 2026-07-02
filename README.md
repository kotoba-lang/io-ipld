# ipld

[![CI](https://github.com/kotoba-lang/ipld/actions/workflows/ci.yml/badge.svg)](https://github.com/kotoba-lang/ipld/actions/workflows/ci.yml)

**The canonical IPLD DAG-CBOR layer for kotoba-lang — real tag-42 CID links,
portable `.cljc`, verified on JVM and real ClojureScript (shadow-cljs).**

Composes [`kotoba-lang/multiformats`](https://github.com/kotoba-lang/multiformats)
(CIDv1 sha2-256 assembly) and [`kotoba-lang/dag-cbor`](https://github.com/kotoba-lang/dag-cbor)
(canonical CBOR + generic tags) into the DAG-CBOR spec's link discipline. This
closes the honesty note the first `prolly-tree`/`quad-store`/`commit-dag`/
`kotoba-client` landing carried — "child/commit references are plain CID
strings, not true tag-42 IPLD links, because `cbor.core` has no tag support".
`cbor.core` now has tags (major type 6), and this repo is the one place that
maps them to links:

- a link encodes as CBOR **tag 42** wrapping `0x00 ++ <binary CID>` — the exact
  wire form generic IPLD tooling (`ipfs dag get`, go-ipld-prime, js dag-cbor)
  expects;
- **only** tag 42 is legal in a block (DAG-CBOR spec): `decode` throws on any
  other tag, `encode` throws on raw `cbor/tagged` values and non-string map keys;
- in application data a link is the explicit `Link` wrapper — `(link cid)`,
  `link?`, `(link-cid l)`; nothing is ever silently a link.

## Use

```clojure
(require '[ipld.core :as ipld])

(def store (atom {}))
(def put!   (fn [cid bytes] (swap! store assoc cid bytes)))
(def get-fn (fn [cid] (get @store cid)))

(def leaf (ipld/put-node! put! {"kind" "leaf" "v" 1}))
(def root (ipld/put-node! put! {"kind" "internal"
                                "children" [["a" (ipld/link leaf)]]}))

(ipld/get-node get-fn root)   ;=> {"kind" "internal", "children" [["a" #ipld/link …]]}
(ipld/links (ipld/get-node get-fn root))  ;=> [leaf-cid] — generic DAG walk,
                                          ;   no node-schema knowledge needed
```

`links` is the one walk hydrate loops and GC need: `kotoba-lang/kotoba-client`
traverses any block graph by `links` alone.

## Consumers

`prolly-tree` (node children), `quad-store` (commit index-roots/prev),
`commit-dag` (prev link), `kotoba-client` (generic missing-blocks walk).
Migrating them from plain CID strings to tag-42 links **changes every CID**
(the encoded bytes change); nothing in production consumes the old-format
blocks (production kotobase runs the wasm build of the deleted Rust engine),
so the switch is a clean break, recorded in the superproject ADR.

## Test

```bash
clojure -M:test       # JVM
npm install && npm run test:cljs   # real ClojureScript (shadow-cljs node-test)
```

## License

MIT
