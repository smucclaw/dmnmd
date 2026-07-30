# dmnmd's native build dependencies, in a form a machine can read.
#
# There are none. This file is deliberately kept, empty of buildInputs, because
# it is the one machine-readable place that records the fact — and because the
# absence is now load-bearing rather than incidental.
#
# It used to carry `pkg-config` and `pcre`, for `regex-pcre`. That dependency was
# retired when the cell layer stopped using regexes: two of its five call sites
# went with the interval recogniser (replaced by DMN.ParseCell's anchored
# grammar), and the other three were inferType classifiers whose nine patterns
# turned out to be eight literal substrings and one anchored digit test.
#
# Why the absence is worth protecting: jl4-wasm build-depends on jl4-core, so
# anything jl4-core might one day depend on has to cross-build for wasm32, and a
# binding to a C library cannot. legalese/l4-ide reached the same conclusion from
# the other direction — see its specs/done/WASM-LSP-SPEC.md, which records
# dropping pcre2 from jl4-core.cabal for exactly that reason. Adding a C-library
# dependency here would close a door that is currently open.
#
# GHC and cabal are deliberately not pinned. Everyone working on this uses ghcup
# or a system toolchain; see `tested-with` in dmnmd.cabal for the version this is
# known to build under (GHC 9.10.3, cabal 3.16).
#
#   nix-shell        # then: cabal build && cabal test && make corpus

{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = [ ];
  buildInputs = [ ];
}
