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

ghcid-tests:
	ghcid --command "stack ghci dmnmd:lib dmnmd:test:dmnmd-test --ghci-options=-fobject-code" --allow-eval --test main

tests-watch:
	# stack test --ta "--fail-fast" --file-watch
	stack build --file-watch --test --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'

# Gives very quick iteration, especially when used with -- $> command
ghcid:
	ghcid --command "stack ghci dmnmd:lib" --allow-eval

# Refresh the DMN->L4 golden fixtures (test/golden/) from the canonical homelab
# sources. Committed copies are used because the homelab repo is not guaranteed
# present in CI (BUILD-SPEC §7.1).
HOMELAB_DOCS = /Users/mengwong/src/mengwong/homelab/docs
sync-golden:
	cp $(HOMELAB_DOCS)/miles-card-dmn.md test/golden/miles-card-dmn.md
	cp $(HOMELAB_DOCS)/miles-card.l4     test/golden/miles-card.l4