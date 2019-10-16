# JITCert

## Installation

If you use the `cabal new-*` family of tools, you should be able to just build
directly using the cabal file, e.g. `cabal new-build`.  You may also wish to
install graphviz in order to deal with the output of dot file backend.

### Nix

Nix will handle dependencies for you, including a working version of GHC and
graphviz.  To build with Nix:

Install [Nix](https://nixos.org/nix/download.html) first, and then do the
following:

    nix-channel --update
    nix-shell --run "cabal new-build"

### Stack

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). 
If you have not installed the required version of `ghc`, you probably will need to run `stack setup`.
To build:

    stack build

To run:
    
    stack exec -- JitCert

## Documentation

Generate images.

    mkdir doc/img
    stack build && stack exec -- jitcert

Build documentation.

    cd doc
    make

