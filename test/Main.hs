{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  BSD LICENSE

  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
  Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
      * All advertising materials mentioning features or use of this
        software must display the following acknowledgment:
          This product includes software developed by Michael Truog
      * The name of the author may not be used to endorse or promote
        products derived from this software without specific prior
        written permission

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.

 -}

import Prelude hiding (id)
import Test.HUnit
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Word as Word
import qualified Foreign.Erlang as Erlang
import qualified Foreign.Erlang.Pid as Erlang
type ByteString = ByteString.ByteString
type Word8 = Word.Word8
type OtpErlangTerm = Erlang.OtpErlangTerm

word8 :: Int -> Word8
word8 value = fromIntegral value

bytes :: String -> ByteString
bytes str = Char8.pack str

pidMake :: Int -> String -> String -> String -> Int -> OtpErlangTerm
pidMake nodeTag node id serial creation =
    Erlang.OtpErlangPid $
        Erlang.Pid
            (word8 nodeTag) (bytes node)
            (bytes id) (bytes serial) (word8 creation)

termOk :: String -> OtpErlangTerm
termOk binary =
    case Erlang.binaryToTerm $ LazyByteString.fromStrict (bytes binary) of
        Left err -> error err
        Right term -> term
        
testPid :: Test
testPid = 
    let pid1 =
            pidMake 100
                "\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e\x6f\x68\x6f\x73\x74"
                "\x00\x00\x00\x3b" "\x00\x00\x00\x00" 0
        test1 = TestCase $ assertEqual "binary->pid"
            (termOk $
                "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F" ++
                "\x68\x6F\x73\x74\x00\x00\x00\x3B\x00\x00\x00\x00\x00")
            pid1
    in
    TestLabel "testPid" (TestList [test1])

main :: IO Counts
main = runTestTT $ TestList [
    testPid]
