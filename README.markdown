Erlang External Term Format for Haskell
=======================================

[![Build Status](https://app.travis-ci.com/okeuday/erlang_hs.svg?branch=master)](https://app.travis-ci.com/okeuday/erlang_hs)

Provides all encoding and decoding for the Erlang External Term Format
(as defined at [http://erlang.org/doc/apps/erts/erl_ext_dist.html](http://erlang.org/doc/apps/erts/erl_ext_dist.html))
in a single Haskell cabal package.

Build/Test
----------

With cabal-install >= 3.4

    cabal v2-update
    mkdir sandbox
    cabal --store-dir=./sandbox v2-configure
    cabal v2-build

With cabal-install >= 2.4 and cabal-install < 3.4

    cabal v1-sandbox init
    cabal v1-update
    cabal v1-install --enable-tests --only-dependencies --force-reinstalls
    cabal v1-configure --enable-tests -v2
    cabal v1-build
    cabal v1-test --show-details=always

With cabal-install < 2.4

    cabal sandbox init
    cabal update
    cabal install --enable-tests --only-dependencies --force-reinstalls
    cabal configure --enable-tests -v2
    cabal build
    cabal test --show-details=always

Without cabal-install

    mkdir -p dist/setup-bin
    ghc --make -outputdir dist/setup-bin -o dist/setup-bin/Setup ./Setup.hs
    dist/setup-bin/Setup configure --builddir=./dist --enable-deterministic --disable-shared --enable-static
    dist/setup-bin/Setup build

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License

