
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