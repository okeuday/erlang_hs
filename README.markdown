Erlang Binary Term Format for Haskell
=====================================

[![Build Status](https://secure.travis-ci.org/okeuday/erlang_hs.png?branch=master)](http://travis-ci.org/okeuday/erlang_hs)

Provides all encoding and decoding for the Erlang Binary Term Format
(as defined at [http://erlang.org/doc/apps/erts/erl_ext_dist.html](http://erlang.org/doc/apps/erts/erl_ext_dist.html))
in a single Haskell cabal package.

Build/Test
----------

    cabal sandbox init
    cabal update
    cabal install --enable-tests --only-dependencies --force-reinstalls
    cabal configure --enable-tests -v2
    cabal build
    cabal test --show-details=always

