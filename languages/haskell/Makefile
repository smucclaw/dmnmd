
ghcid-tests:
	ghcid --command "stack ghci dmnmd:lib dmnmd:test:dmnmd-test --ghci-options=-fobject-code" --allow-eval --test main

tests-watch:
	stack test --ta "--fail-fast" --file-watch

# Gives very quick iteration, especially when used with -- $> command
ghcid:
	ghcid --command "stack ghci dmnmd:lib" --allow-eval