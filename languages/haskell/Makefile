
ghcid-tests:
	ghcid --command "stack ghci dmnmd:lib dmnmd:test:dmnmd-test --ghci-options=-fobject-code" --allow-eval --test main

tests-watch:
	# stack test --ta "--fail-fast" --file-watch
	stack build --file-watch --test --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'

# Gives very quick iteration, especially when used with -- $> command
ghcid:
	ghcid --command "stack ghci dmnmd:lib" --allow-eval