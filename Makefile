test:
	cabal test

test-all-ghcs:
	@for i in 92 94 96 98 910; do (\
		nix develop \
			$${FLAKE_OVERRIDE_NIXPKGS:+--override-input nixpkgs "$${FLAKE_OVERRIDE_NIXPKGS}"} \
			.#ci-ghc$${i} \
			-c -- bash -c "ghc --version; cabal clean; cabal test" \
	) done

.PHONY: test test-*
