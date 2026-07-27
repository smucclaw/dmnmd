.PHONY: ghcid-tests tests-watch ghcid sync-golden corpus corpus-policy corpus-list corpus-record

# ---------------------------------------------------------------- behavioural corpus
#
# test/corpus/ records what dmnmd actually does today, one small input per verified
# defect, classified symptom (should change) or policy (must not). See
# test/corpus/README.md. It shells out to the built binary and does NOT build, so
# `cabal build` first. It is deliberately not part of `cabal test`; the README says
# why.
#
#   make corpus         every case; nonzero only on a POLICY divergence
#   make corpus-policy  just the regression net — the one to put in CI
#   make corpus-list    what is in there
#   make corpus-record  re-record expectations. Read the diff before you do this.

corpus:
	./test/corpus/run-corpus.sh

corpus-policy:
	./test/corpus/run-corpus.sh --class policy

corpus-list:
	./test/corpus/run-corpus.sh --list

corpus-record:
	./test/corpus/run-corpus.sh --record

# ---------------------------------------------------------------- watch loops
#
# These were `stack ghci` / `stack build --file-watch` before the cabal-only
# move. cabal has no --file-watch, so all three go through ghcid, and the
# component split matters:
#
#   `cabal repl test:dmnmd-test` does NOT load src/ — the library arrives as a
#   built dependency, so editing a module under src/ changes nothing in the
#   session. Verified: `:show modules` lists 0 files under src/ there, and 13
#   under --enable-multi-repl. That is why ghcid-tests uses multi-repl and
#   tests-watch uses --restart=src.

# Typechecks the library and the test suite together, so a change under src/
# reloads the tests. Cannot RUN them: GHC's multi-component repl rejects :main
# ("Command is not supported (yet) in multi-mode").
ghcid-tests:
	ghcid --command "cabal repl --enable-multi-repl lib:dmnmd test:dmnmd-test" --allow-eval

# Runs the suite on every change, failures first. Single-component, so a src/
# edit cannot hot-reload — --restart tears the session down and lets cabal
# rebuild the library, which is slower but correct.
tests-watch:
	ghcid --command "cabal repl test:dmnmd-test" \
	  --restart=src --restart=dmnmd.cabal \
	  --test ':main --rerun --failure-report=TESTREPORT --rerun-all-on-success'

# Gives very quick iteration, especially when used with -- $> command
ghcid:
	ghcid --command "cabal repl lib:dmnmd" --allow-eval

# Refresh the DMN->L4 golden fixtures (test/golden/) from the canonical homelab
# sources. Committed copies are used because the homelab repo is not guaranteed
# present in CI (BUILD-SPEC §7.1).
HOMELAB_DOCS = /Users/mengwong/src/mengwong/homelab/docs
sync-golden:
	cp $(HOMELAB_DOCS)/miles-card-dmn.md test/golden/miles-card-dmn.md
	cp $(HOMELAB_DOCS)/miles-card.l4     test/golden/miles-card.l4