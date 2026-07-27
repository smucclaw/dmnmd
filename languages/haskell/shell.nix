# dmnmd's native build dependencies, in a form a machine can read.
#
# This replaces the `nix: pure: true` stanza that stack.yaml carried before the
# cabal-only move. That stanza was dmnmd's only machine-readable record of the
# fact that the build needs pcre — CI's apt line and the README's brew line are
# both prose, and prose does not install anything.
#
# What needs them: regex-pcre links against libpcre and locates it through
# pkg-config. Without both, the *dependency* fails to configure, before any
# project code is compiled — which reads as a mysterious failure in a package
# nobody here wrote. That was one of the two causes of the 2025-06-29 CI outage.
#
# GHC and cabal are deliberately not pinned here. Everyone working on this uses
# ghcup or a system toolchain; see `tested-with` in dmnmd.cabal for the version
# this is known to build under (GHC 9.10.3, cabal 3.16).
#
#   nix-shell        # then: cabal build && cabal test && make corpus
#
# CAVEAT: nothing exercises this file. CI installs the same two packages with
# apt, and no job runs nix-shell, so this can rot without anything going red.
# If it does not work, fix it — do not conclude the dependency list is wrong.

{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.pcre ];
}
