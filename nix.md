### Usage

Investigate targets:

```sh
nix flake show --allow-import-from-derivation
git+file:///home/netsu/Projects/market-maker
├───apps
│   ├───aarch64-darwin
│   │   ├───"geniusyield-annset:lib:geniusyield-annset": app
│   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": app
│   │   ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": app
│   │   └───"geniusyield-market-maker:test:pproviders-status-sequence": app
│   ├───x86_64-darwin
│   │   ├───"geniusyield-annset:lib:geniusyield-annset": app
│   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": app
│   │   ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": app
│   │   └───"geniusyield-market-maker:test:pproviders-status-sequence": app
│   └───x86_64-linux
│       ├───"geniusyield-annset:lib:geniusyield-annset": app
│       ├───"geniusyield-annset:test:geniusyield-annset-tests": app
│       ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": app
│       └───"geniusyield-market-maker:test:pproviders-status-sequence": app
├───checks
│   ├───aarch64-darwin
│   │   ├───"geniusyield-annset:test:geniusyield-annset-tests" omitted (use '--all-systems' to show)
│   │   └───"geniusyield-market-maker:test:pproviders-status-sequence" omitted (use '--all-systems' to show)
│   ├───x86_64-darwin
│   │   ├───"geniusyield-annset:test:geniusyield-annset-tests" omitted (use '--all-systems' to show)
│   │   └───"geniusyield-market-maker:test:pproviders-status-sequence" omitted (use '--all-systems' to show)
│   └───x86_64-linux
│       ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0-check'
│       └───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0-check'
├───ciJobs: unknown
├───devShell
│   ├───aarch64-darwin omitted (use '--all-systems' to show)
│   ├───x86_64-darwin omitted (use '--all-systems' to show)
│   └───x86_64-linux: development environment 'ghc-shell-for-packages'
├───devShells
│   ├───aarch64-darwin
│   │   └───default omitted (use '--all-systems' to show)
│   ├───x86_64-darwin
│   │   └───default omitted (use '--all-systems' to show)
│   └───x86_64-linux
│       └───default: development environment 'ghc-shell-for-packages'
├───hydraJobs
│   ├───aarch64-darwin
│   │   ├───checks
│   │   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0-check'
│   │   │   └───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0-check'
│   │   ├───coverage
│   │   ├───devShells
│   │   │   └───default: derivation 'ghc-shell-for-packages'
│   │   ├───packages
│   │   │   ├───"geniusyield-annset:lib:geniusyield-annset": derivation 'geniusyield-annset-lib-geniusyield-annset-0.1.0.0'
│   │   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0'
│   │   │   ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": derivation 'geniusyield-market-maker-exe-geniusyield-market-maker-exe-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:datasource-providers": derivation 'geniusyield-market-maker-lib-datasource-providers-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:geniusyield-market-maker-lib": derivation 'geniusyield-market-maker-lib-geniusyield-market-maker-lib-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:orderbook-annset": derivation 'geniusyield-market-maker-lib-orderbook-annset-0.8.0'
│   │   │   ├───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0'
│   │   │   ├───"geniusyield-orderbot-framework:lib:common": derivation 'geniusyield-orderbot-framework-lib-common-0.1.0.0'
│   │   │   ├───"geniusyield-orderbot-framework:lib:datasource": derivation 'geniusyield-orderbot-framework-lib-datasource-0.1.0.0'
│   │   │   └───"geniusyield-orderbot-framework:lib:orderbook": derivation 'geniusyield-orderbot-framework-lib-orderbook-0.1.0.0'
│   │   ├───plan-nix: derivation 'haskell-project-plan-to-nix-pkgs'
│   │   └───roots: derivation 'haskell-nix-roots-ghc966'
│   ├───x86_64-darwin
│   │   ├───checks
│   │   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0-check'
│   │   │   └───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0-check'
│   │   ├───coverage
│   │   ├───devShells
│   │   │   └───default: derivation 'ghc-shell-for-packages'
│   │   ├───packages
│   │   │   ├───"geniusyield-annset:lib:geniusyield-annset": derivation 'geniusyield-annset-lib-geniusyield-annset-0.1.0.0'
│   │   │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0'
│   │   │   ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": derivation 'geniusyield-market-maker-exe-geniusyield-market-maker-exe-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:datasource-providers": derivation 'geniusyield-market-maker-lib-datasource-providers-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:geniusyield-market-maker-lib": derivation 'geniusyield-market-maker-lib-geniusyield-market-maker-lib-0.8.0'
│   │   │   ├───"geniusyield-market-maker:lib:orderbook-annset": derivation 'geniusyield-market-maker-lib-orderbook-annset-0.8.0'
│   │   │   ├───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0'
│   │   │   ├───"geniusyield-orderbot-framework:lib:common": derivation 'geniusyield-orderbot-framework-lib-common-0.1.0.0'
│   │   │   ├───"geniusyield-orderbot-framework:lib:datasource": derivation 'geniusyield-orderbot-framework-lib-datasource-0.1.0.0'
│   │   │   └───"geniusyield-orderbot-framework:lib:orderbook": derivation 'geniusyield-orderbot-framework-lib-orderbook-0.1.0.0'
│   │   ├───plan-nix: derivation 'haskell-project-plan-to-nix-pkgs'
│   │   └───roots: derivation 'haskell-nix-roots-ghc966'
│   └───x86_64-linux
│       ├───checks
│       │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0-check'
│       │   └───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0-check'
│       ├───coverage
│       ├───devShells
│       │   └───default: derivation 'ghc-shell-for-packages'
│       ├───packages
│       │   ├───"geniusyield-annset:lib:geniusyield-annset": derivation 'geniusyield-annset-lib-geniusyield-annset-0.1.0.0'
│       │   ├───"geniusyield-annset:test:geniusyield-annset-tests": derivation 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0'
│       │   ├───"geniusyield-market-maker:exe:geniusyield-market-maker-exe": derivation 'geniusyield-market-maker-exe-geniusyield-market-maker-exe-0.8.0'
│       │   ├───"geniusyield-market-maker:lib:datasource-providers": derivation 'geniusyield-market-maker-lib-datasource-providers-0.8.0'
│       │   ├───"geniusyield-market-maker:lib:geniusyield-market-maker-lib": derivation 'geniusyield-market-maker-lib-geniusyield-market-maker-lib-0.8.0'
│       │   ├───"geniusyield-market-maker:lib:orderbook-annset": derivation 'geniusyield-market-maker-lib-orderbook-annset-0.8.0'
│       │   ├───"geniusyield-market-maker:test:pproviders-status-sequence": derivation 'geniusyield-market-maker-test-pproviders-status-sequence-0.8.0'
│       │   ├───"geniusyield-orderbot-framework:lib:common": derivation 'geniusyield-orderbot-framework-lib-common-0.1.0.0'
│       │   ├───"geniusyield-orderbot-framework:lib:datasource": derivation 'geniusyield-orderbot-framework-lib-datasource-0.1.0.0'
│       │   └───"geniusyield-orderbot-framework:lib:orderbook": derivation 'geniusyield-orderbot-framework-lib-orderbook-0.1.0.0'
│       ├───plan-nix: derivation 'haskell-project-plan-to-nix-pkgs'
│       └───roots: derivation 'haskell-nix-roots-ghc966'
├───legacyPackages
│   ├───aarch64-darwin omitted (use '--legacy' to show)
│   ├───x86_64-darwin omitted (use '--legacy' to show)
│   └───x86_64-linux omitted (use '--legacy' to show)
└───packages
    ├───aarch64-darwin
    │   └───default omitted (use '--all-systems' to show)
    ├───x86_64-darwin
    │   └───default omitted (use '--all-systems' to show)
    └───x86_64-linux
        └───default: package 'geniusyield-annset-test-geniusyield-annset-tests-0.1.0.0'
```

To open shell:

```sh
nix develop
```

It supposed to be that in the shell the `cabal update` should be not necessary!  
But at the moment, there's a bug: https://github.com/IntersectMBO/cardano-cli/issues/533#issuecomment-2014821037

```sh
cabal update
cabal build all
```

To build:


```sh
nix build .
```

### Known limitations

- some birary caches are not useful now, because of fixes of dependencies, so each cabal update leads to the massive recompilation ☹️
- cross-compilation possible but not implemented (yet)
- cabal inside the shell rebuild available haskell packages

Feel free to add any useful run scripts to the flake, please!

### Troubleshooting

Check cabal file:

```sh
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/041c867bad68dfe34b78b2813028a2e2ea70a23c.tar.gz -p haskellPackages.cabal-install haskellPackages.ghc --run "cabal new-configure"
```

Check build:

```sh
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/041c867bad68dfe34b78b2813028a2e2ea70a23c.tar.gz -p haskellPackages.cabal-install -p libsodium -p blst -p secp256k1 -p zlib -p postgresql -p pkg-config haskellPackages.ghc --run "cabal build all"
```

Check cabal2nix:

```sh
nix build -f https://github.com/input-output-hk/haskell.nix/archive/94a061666cbd39c32468efed72dca29cf716c90a.tar.gz pkgs.haskell-nix.nix-tools.ghc966 --out-link nt
```

Elaborate derivation:

```sh
nix-shell  -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/041c867bad68dfe34b78b2813028a2e2ea70a23c.tar.gz -p nix-prefetch-git --run "./nt/bin/plan-to-nix --output ./. --plan-json ./dist-newstyle/cache/plan.json --cabal-project geniusyield-market-maker/geniusyield-market-maker.cabal"
```

Investigate inputs:

```sh
nix --extra-experimental-features repl-flake repl .
> packages.x86_64-linux.geniusyield-
packages.x86_64-linux.geniusyield-annset:lib:geniusyield-annset                  packages.x86_64-linux.geniusyield-market-maker:test:pproviders-status-sequence
packages.x86_64-linux.geniusyield-annset:test:geniusyield-annset-tests           packages.x86_64-linux.geniusyield-orderbot-framework:lib:common
packages.x86_64-linux.geniusyield-market-maker:exe:geniusyield-market-maker-exe  packages.x86_64-linux.geniusyield-orderbot-framework:lib:datasource
packages.x86_64-linux.geniusyield-market-maker:lib:datasource-providers          packages.x86_64-linux.geniusyield-orderbot-framework:lib:orderbook
packages.x86_64-linux.geniusyield-market-maker:lib:geniusyield-market-maker-lib
packages.x86_64-linux.geniusyield-market-maker:lib:orderbook-annset
```





