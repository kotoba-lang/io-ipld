# ipld

**The canonical IPLD DAG-CBOR layer for kotoba-lang — real tag-42 CID links,
portable `.cljc`, verified on JVM, SCI/nbb, and compiled ClojureScript.**

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

(ipld/get-node get-fn root)   ;=> {"kind" "internal", "children" [["a" (ipld/link leaf)]]}
(ipld/links (ipld/get-node get-fn root))  ;=> [leaf-cid] — generic DAG walk,
                                          ;   no node-schema knowledge needed
```

`links` is the one walk hydrate loops and GC need: `kotoba-lang/kotoba-client`
traverses any block graph by `links` alone.

## DAG-PB

`ipld.dag-pb` is the other codec — the one UnixFS is written in, and the one
where the `.proto` is not enough to produce a correct block. Two rules come
from the reference implementation rather than the schema:

- **`Links` is field 2, `Data` is field 1, and `Links` is written first.**
  Wire order is descending. An encoder that emits ascending field numbers —
  which is what a deterministic protobuf library correctly does — produces a
  different block and therefore a different CID.
- **`Name` is written even when empty.** Protobuf omits default-valued
  optional fields; go-merkledag writes `12 00` on every file chunk link, and
  the block is hashed as written.

```clojure
(require '[ipld.dag-pb :as dag-pb])

(dag-pb/node->block {:links [{:hash leaf-cid :name "" :tsize 262144}]
                     :data unixfs-file-header})
;; => {:cid "bafybei…" :bytes #object[byte[]]}

(dag-pb/decode block-bytes)  ; => {:links [{:cid :hash :name :tsize}] :data …}
```

Encoding is explicit here; only decoding delegates to `protobuf.wire`, whose
any-order reading and unknown-field preservation are exactly right for blocks
written by other implementations. The fixtures in `ipld.dag-pb-test` are real
kubo 0.41 blocks pinned as hex — not output of this encoder. A codec checked
only against itself is self-consistent, which is not the property anyone wants
from a codec.

## IPLD layers

This repository now keeps IPLD's layers distinct instead of treating CBOR as
the whole stack:

| namespace | responsibility |
|---|---|
| `ipld.data-model` | the nine Data Model kinds, lossless validation, and the universal `INode` interface used by native values and ADLs |
| `ipld.core` | strict DAG-CBOR representation plus CID-verified block reads |
| `ipld.schema-dsl` | the user-facing IPLD Schema syntax compiled into normalized Schema DMT |
| `ipld.schema` | Schema-Schema DMT shape/reference validation and bounded representation unification, including metered ADL capabilities |
| `ipld.selector` | non-conditional IPLD selectors over native values or ADL Nodes, including bounded recursion, transparent Link resolution, and strict Data Model/DAG-CBOR codecs |
| `ipld.graph` | bounded selector execution with root-first, deduplicated proof blocks suitable for CAR/GraphSync adapters |

Schema DMT is the runtime source of truth. `ipld.schema-dsl/parse` handles the
prelude scalar types, named copies, typed/untyped links, inline maps/lists,
nullable values, optional fields, map/tuple structs with field rename or
implicit annotations, all six union representations (keyed, kinded, envelope,
inline, stringprefix, and bytesprefix), string/int enums, unit/any types, and
declared advanced representations. Bare `bytes` definitions normalize to the
Schema-Schema DMT's explicit `{"representation" {"bytes" {}}}` form.
`ipld.schema/compile-schema` validates exact required/optional keys, nested
definition shapes, representation tables, enum mappings, string-kind map keys,
copy cycles, advanced declarations, and every named reference before returning
a compiled schema. `unify!` matches representation values under mandatory
depth/node budgets and requires a caller-owned validator capability before an
advanced representation can execute. Tuple and stringjoin `fieldOrder`, map
and struct stringpairs, struct stringjoin/listpairs, and typed scalar implicit
values are executable. `representation->logical!` restores logical struct
field names and implicit values; `logical->representation!` performs the
inverse projection and validates its output. All six union representations use
the stable logical shape `{:member TypeNameOrInlineDefn :value value}` and
recursively project their selected member. Delimiter-based representations
fail closed on duplicate keys, non-canonical numeric text, and unescaped
delimiter collisions. Advanced map/list/bytes representations execute only
through caller-owned `:adl-capabilities` containing explicit representation
validation and `:decode`/`:encode` functions (with optional logical
validation). Capability execution additionally requires positive
`:max-adl-fuel`, `:max-adl-output-nodes`, and `:max-adl-output-bytes` limits.
Each operation is charged its declared `:fuel-cost` or operation-specific
`:fuel-costs`, input/output Data Model size is measured, and projection results
contain `:adl-fuel-used` plus ordered `:adl-receipts`. Setting
`:check-adl-determinism? true` actively runs encode/decode twice and rejects
different Data Model outputs. Fuel is a caller-declared boundary cost: opaque
Clojure functions are not instruction-preempted, so untrusted transforms still
belong in a separately metered Wasm/process backend. The legacy
`:adl-validators` validation-only API remains compatible.

`ipld.schema/wasm-adl-capability` defines that backend boundary as
`ipld-adl-wasm-v1`. The capability pins raw module bytes to a raw CID and an
explicit operation set. Its trusted engine receives only canonical DAG-CBOR
input plus the remaining fuel, maximum output bytes, and maximum memory pages;
there is no ambient filesystem, network, clock, randomness, or host callback in
the schema API. A successful engine response must be exactly
`{:status :ok :engine-id ... :module-cid ... :output-bytes ... :fuel-used ...
:memory-pages ...}`. The schema layer independently checks the Wasm
magic/version, declared engine identity, and module CID, caps module/output
bytes and memory pages, charges the engine-measured fuel, rejects non-canonical
DAG-CBOR output, and records the
module plus input/output CIDs in each receipt. Wasmtime, Kotoba's runtime, or a
worker process can implement this synchronous port; the engine remains part of
the trusted computing base because only it can measure guest instructions.

`ipld.graph/select-blocks` is intentionally transport-neutral. It requires
explicit block, byte, path-depth, and match limits; rehashes every fetched
block; and returns the exact root-first block sequence a CAR writer needs.
For backpressured transports, `selection-cursor` creates a read-free,
checkpointable traversal and `advance-cursor` performs at most one new
CID-verified block read under an explicit CPU work budget. Decoded nodes and
selector work remain in the returned immutable cursor, so cancellation does
not require another read and resumption does not replay prior storage reads.
`resolve-path` compiles a Data Model path to a selector and returns its proof
blocks. This is the shared correctness core for:

- an IPFS trustless HTTP gateway adapter that parses HTTP path/range/content
  negotiation and frames the result as CAR;
- a GraphSync adapter that maps a wire request's root and selector into this
  bounded traversal and maps the result into response messages.

Neither transport wire protocol is claimed here. `ipld.selector/encode` and
`decode` serialize Matcher (including labels), ExploreAll, ExploreFields,
ExploreIndex, ExploreRange, ExploreUnion, ExploreRecursive, and
ExploreRecursiveEdge using the compact IPLD Selector schema. Recursive
execution is available only through bounded `select-graph`; finite depth and
`none` recursion are both still fenced by the traversal's mandatory block,
byte, path-depth, and match limits. The draft Condition algebra remains
excluded because the upstream specification itself still marks it incomplete;
conditions are rejected rather than approximated.

## Canonical Kotoba values

Compiler, provider, actor, and I/O boundaries should use the language-facing
namespace rather than couple themselves to the IPLD node API. Ability
boundaries use the bounded operations; the ability descriptor supplies the
limit and the codec enforces it before data crosses the boundary:

```clojure
(require '[kotoba.value.codec :as value])

(def bytes (value/encode-bounded {:actor/id 7 :ready true} 4096))
(value/decode-bounded bytes 4096) ;=> {:actor/id 7, :ready true}

;; Full signed i64 is explicit at the wire adapter. Bare integers retain the
;; cross-runtime JS-safe contract.
(value/encode-bounded (value/int64 9223372036854775807) 16) ; JVM
;; ClojureScript adapters pass (js/BigInt "9223372036854775807").
```

The codec id is `kotoba.value.v1`. `kotoba.value.codec` is a stable facade over
the existing, cross-runtime-qualified `ipld.value` implementation; this change
does not introduce a second wire format or duplicate its encoder. Value-only
CLJS consumers load the lightweight Link/base32 adapter and do not need the
SHA/npm implementation used by content-addressing operations in `ipld.core`.
Exact i64 uses append-only scalar code `9` with an 8-byte big-endian signed
two's-complement payload; it never routes a JavaScript BigInt through CBOR's
Number integer path.

## Consumers

`prolly-tree` (node children), `quad-store` (commit index-roots/prev),
`commit-dag` (prev link), `kotoba-client` (generic missing-blocks walk).
Migrating them from plain CID strings to tag-42 links **changes every CID**
(the encoded bytes change); nothing in production consumes the old-format
blocks (production kotobase runs the wasm build of the deleted Rust engine),
so the switch is a clean break, recorded in the superproject ADR.

## Test

```bash
clojure -M:test                  # JVM
npm run test:nbb                 # SCI/nbb portability boundary
npm install && npm run test:cljs # nbb + compiled ClojureScript node-test
```

## License

MIT
