# visitors-cleanroom

A drop-in replacement for the upstream
[visitors](https://gitlab.inria.fr/fpottier/visitors) OCaml package.
It provides:

1. **A clean-room runtime** (`visitors-cleanroom` library) -- an
   independently-written `VisitorsRuntime` module, licensed LGPL-2.1
   with the OCaml linking exception.

2. **The upstream PPX** (`visitors-cleanroom.ppx`) -- built from a git
   submodule, configured to depend on our runtime instead of
   `visitors.runtime`.

## Getting started

```sh
git clone --recurse-submodules https://github.com/semgrep/visitors-cleanroom.git
cd visitors-cleanroom
opam install ppxlib ppx_deriving result
dune build
```

If you already cloned without `--recurse-submodules`, run
`git submodule update --init`.

## Testing

Tests compare our runtime against the upstream `visitors.runtime` on
random inputs:

```sh
opam install visitors qcheck-core qcheck-alcotest alcotest
dune test
```

Passing tests produce no output. Use `dune test --force` to see
results unconditionally.

## Benchmarks

```sh
opam install bechamel
dune exec bench/bench.exe          # OO visitor benchmarks + stack depth conformance
dune exec bench/bench_functors.exe # functor-based visitor benchmarks
```

`bench_functors` compares PPX-generated OO visitors against hand-written
functor-based visitors on the same types, measuring the overhead of
virtual dispatch vs. static functor application.

## Usage

```dune
(library
 (name my_lib)
 (preprocess (pps visitors-cleanroom.ppx)))
```

The PPX automatically links our clean-room `VisitorsRuntime` via
`ppx_runtime_deps`.

## ppxlib compatibility

The upstream PPX submodule (`vendor/visitors/`, tag `20250212`) requires ppxlib
< 0.36.0. ppxlib 0.36.0 added `Ptyp_open` to its public AST. The upstream
visitors release `20251114` handles `Ptyp_open`, but requires ppxlib >= 0.36.0.
To upgrade, bump both together:

1. `cd vendor/visitors && git checkout 20251114`
2. Change the ppxlib constraint in `dune-project` to `(>= 0.36.0)`.

## License

The clean-room runtime (`lib/`) is licensed LGPL-2.1 with the OCaml
linking exception (Copyright 2026, Semgrep Inc.). The PPX
(`vendor/visitors/`) is the upstream source, licensed LGPL-2.1
(Copyright Francois Pottier / INRIA).
