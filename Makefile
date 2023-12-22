.PHONY: format lint requires_nix_shell

EXTENSIONS := -o -XTypeApplications -o -XPatternSynonyms -o -XBangPatterns -o -XUnboxedTuples

# MARKET MAKER

makerbot-maestro:
	cabal run geniusyield-market-maker-exe -- Run config-maestro.json maker-bot-config.json

makerbot-blockfrost:
	cabal run geniusyield-market-maker-exe -- Run config-blockfrost.json maker-bot-config.json

cancel-maestro:
	cabal run geniusyield-market-maker-exe -- Cancel config-maestro.json maker-bot-config.json

cancel-blockfrost:
	cabal run geniusyield-market-maker-exe -- Cancel config-blockfrost.json maker-bot-config.json

# Run formatters
format: requires_nix_shell
	fourmolu -i --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs -E geniusyield-framework-subtr)

# Check formatting (without making changes)
format_check:
	fourmolu --mode check --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs -E geniusyield-framework-subtr)

# Apply hlint suggestions
lint: requires_nix_shell
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check: requires_nix_shell
	hlint $(shell fd -ehs)

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)
