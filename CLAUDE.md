# CLAUDE.md

## What this project is

A drop-in replacement for the upstream `visitors` OCaml package that lets you
statically link the runtime into proprietary binaries without LGPL issues. See
README.md for usage.

## Architecture

The upstream `visitors` package ships two sub-libraries:

- `visitors.ppx` -- a `ppx_deriving` plugin that generates visitor classes from
  `[@@deriving visitors]` annotations.
- `visitors.runtime` -- a single module, `VisitorsRuntime`, providing base
  visitor classes (`iter`, `map`, `endo`, `reduce`, `mapreduce`, and arity-2
  variants) that generated code inherits from.

The PPX declares `ppx_runtime_deps visitors.runtime`, which forces dune to
auto-link the upstream runtime whenever the PPX is used. This coupling is the
core problem.

### How we break the coupling

`visitors-cleanroom` builds the upstream PPX from a git submodule
(`vendor/visitors/`, pinned to tag `20250212`) but declares `ppx_runtime_deps`
pointing at our clean-room runtime instead.

Key dune mechanics:

- `vendor/dune` uses `(data_only_dirs visitors)` so dune does not descend into
  the submodule or try to build its dune-project (which would create a
  duplicate `VisitorsRuntime` module).
- `ppx/dune` uses `(copy_files ../vendor/visitors/src/*.ml)` and `(copy_files
  ../vendor/visitors/src/*.mli)` to pull the PPX sources into our build, where
  we control the library definition.
- `ppx/dune` declares `(ppx_runtime_libraries visitorsRuntime)`, pointing at
  our runtime, not the upstream one.

### Sub-libraries

| Public name                  | Directory    | What it provides |
|------------------------------|--------------|------------------|
| `visitors-cleanroom`         | `lib/`       | `Visitors_reimpl_runtime` module (clean-room) |
| `visitors-cleanroom.runtime` | `lib/alias/` | `VisitorsRuntime` alias (re-exports above) |
| `visitors-cleanroom.ppx`     | `ppx/`       | PPX deriver (upstream source, our runtime dep) |

The alias library exists because the upstream PPX hardcodes the module name
`VisitorsRuntime` in generated code. The core implementation uses a distinct
name (`Visitors_reimpl_runtime`) so that tests can link both our runtime and
upstream's without module shadowing.

## Project layout

```
lib/
  visitors_reimpl_runtime.ml   -- clean-room runtime implementation
  visitors_reimpl_runtime.mli  -- public interface with semantic contracts
  dune                            -- public_name: visitors-cleanroom
  alias/
    visitorsRuntime.ml            -- include Visitors_reimpl_runtime
    dune                          -- public_name: visitors-cleanroom.runtime

ppx/
  dune                  -- public_name: visitors-cleanroom.ppx
                        -- builds from vendor/visitors/src/*.ml via copy_files
                        -- ppx_runtime_libraries: visitorsRuntime (alias)

vendor/
  visitors/             -- git submodule (gitlab.inria.fr/fpottier/visitors)
                        -- pinned to tag 20250212
  dune                  -- (data_only_dirs visitors)

test/
  test_visitors_runtime.ml  -- 118 property-based tests: base class methods
  test_traversal.ml         -- 32 property-based tests: PPX-generated traversals
  test_types.ml             -- shared ADT definitions (symlinked into subdirs)
  types_ours/               -- compiles test_types.ml linking our runtime
    visitorsRuntime.ml      -- local bridge: include Visitors_reimpl_runtime
  types_theirs/             -- compiles test_types.ml linking upstream runtime
  upstream/                 -- re-exports upstream VisitorsRuntime as
                               Upstream_visitors_runtime to avoid name collisions

bench/
  bench.ml              -- bechamel benchmarks + stack depth conformance
  bench_functors.ml     -- functor-based visitor benchmarks
```

## Build commands

- `dune build` -- build the library and PPX
- `dune build -p visitors-cleanroom` -- build in package/release mode (as opam would)
- `dune test` -- run conformance tests (requires upstream `visitors` installed)
- `dune test --force` -- re-run tests even if nothing changed
- `dune exec bench/bench.exe` -- run benchmarks (requires `bechamel`)

## Test architecture

Tests compare our runtime against upstream on random inputs using QCheck. The
same `test_types.ml` source is compiled twice:

- `types_ours` links against `visitors_reimpl_runtime` (our runtime).
  A local `visitorsRuntime.ml` bridge (`include Visitors_reimpl_runtime`)
  ensures PPX-generated `VisitorsRuntime.*` references resolve to our code.
- `types_theirs` links against `visitors.runtime` (upstream)

Both use `visitors.ppx` for code generation. The test runner instantiates
visitors from both and asserts identical behavior.

`upstream/upstream_visitors_runtime.ml` is `include VisitorsRuntime` -- it
re-exports the upstream module under a distinct name so test code can reference
both runtimes without ambiguity.

The distinct module name `Visitors_reimpl_runtime` is critical for test
correctness. Previously, both our library and upstream exposed `VisitorsRuntime`,
causing module shadowing: tests compared upstream against itself, masking bugs.

## Implementation contracts

It is very important that the upstream VisitorsRuntime never be inadvertedly
used outside the integration test suite.  When users' compiled programs are
executing they must only use *this* VisitorsRuntime.

The `.mli` file documents these; they are the main things to get right
if modifying the runtime:

- `visit_list` must use recursive self-calls (`self#visit_list`), not
  `List.iter`/`List.map`, so subclass overrides aren't bypassed.
- `endo#visit_lazy_t` must eagerly force the suspension and preserve physical
  identity (`==`) when unchanged.
- `mapreduce#visit_list` must use a right fold (head, recurse tail, then
  combine) so non-associative monoids get correct association.
- `array_equal` asserts equal length rather than returning false.
- Arity-2 `visit_float` with NaN raises `StructuralMismatch`.

If, on subsequent changes, your integration tests discover a mismatch in
behaviour between our runtime and upstream, fix the clean-room implementation
to match upstream's semantics.

## Integration with other projects

To replace upstream `visitors` in your projects:

1. Add `pin-depends` in `/the/path/to/your/opam.template`:
   `["visitors-cleanroom.dev" "git+https://github.com/semgrep/visitors-cleanroom.git#COMMIT"]`
2. Replace `visitors` with `visitors-cleanroom` in `the/path/to/your/dune-project` deps.
3. Change `(pps visitors.ppx)` to `(pps visitors-cleanroom.ppx)` in `/the/path/to/the/relevant/dune`
4. Remove the upstream `visitors` opam dependency.
5. No OCaml code changes needed -- `VisitorsRuntime` module name matches.
