Erlang Binary Term Format for Haskell
=====================================

**WIP**

[![Build Status](https://secure.travis-ci.org/okeuday/erlang_hs.png?branch=master)](http://travis-ci.org/okeuday/erlang_hs)

Provides all encoding and decoding for the Erlang Binary Term Format
(as defined at [http://erlang.org/doc/apps/erts/erl_ext_dist.html](http://erlang.org/doc/apps/erts/erl_ext_dist.html))
in a single Haskell cabal package.

Build/Test
----------

    cabal sandbox init
    cabal update
    cabal install binary-0.8.4.1 containers-0.5.10.1 test-framework-0.8.1.1 test-framework-hunit-0.3.0.2 zlib --force-reinstalls
    cabal configure --enable-tests
    cabal build
    cabal test

