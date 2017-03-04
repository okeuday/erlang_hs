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
import System.Exit (exitWith,ExitCode(..))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Word as Word
import qualified Foreign.Erlang as Erlang
import qualified Foreign.Erlang.Pid as E
import qualified Foreign.Erlang.Function as E
import qualified Foreign.Erlang.Reference as E
type ByteString = ByteString.ByteString
type Word8 = Word.Word8
type OtpErlangTerm = Erlang.OtpErlangTerm

word8 :: Int -> Word8
word8 value = fromIntegral value

bytes :: String -> ByteString
bytes str = Char8.pack str

makePid :: Int -> String -> String -> String -> Int -> OtpErlangTerm
makePid nodeTag node id serial creation =
    Erlang.OtpErlangPid $
        E.Pid (word8 nodeTag) (bytes node)
            (bytes id) (bytes serial) (word8 creation)

makeFunction :: Int -> String -> OtpErlangTerm
makeFunction tag value =
    Erlang.OtpErlangFunction $
        E.Function (word8 tag) (bytes value)

makeReference :: Int -> String -> String -> Int -> OtpErlangTerm
makeReference nodeTag node id creation =
    Erlang.OtpErlangReference $
        E.Reference (word8 nodeTag) (bytes node) (bytes id) (word8 creation)

termOk :: String -> OtpErlangTerm
termOk binary =
    case Erlang.binaryToTerm $ LazyByteString.fromStrict (bytes binary) of
        Left err -> error $ show err
        Right term -> term

termError :: String -> String
termError binary =
    case Erlang.binaryToTerm $ LazyByteString.fromStrict (bytes binary) of
        Left err -> show err
        Right term -> error $ show term
        
binaryOk :: OtpErlangTerm -> String
binaryOk term =
    case Erlang.termToBinary term (-1) of
        Left err -> error $ show err
        Right binary -> LazyChar8.unpack binary
        
testPid :: Test
testPid = 
    let pid1 =
            makePid 100
                "\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e\x6f\x68\x6f\x73\x74"
                "\x00\x00\x00\x3b" "\x00\x00\x00\x00" 0
        binary = "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F" ++
                 "\x68\x6F\x73\x74\x00\x00\x00\x3B\x00\x00\x00\x00\x00"
        test1 = TestCase $ assertEqual "pid->binary"
            (binaryOk $ pid1) binary
        test2 = TestCase $ assertEqual "binary->pid"
            (termOk $ binary) pid1
    in
    TestLabel "testPid" (TestList [test1, test2])

testFunction :: Test
testFunction = 
    let function1 =
            makeFunction 113
                ("\x64\x00\x05\x6c\x69\x73\x74\x73\x64" ++
                 "\x00\x06\x6d\x65\x6d\x62\x65\x72\x61\x02")
        binary = "\x83\x71\x64\x00\x05\x6C\x69\x73\x74\x73\x64\x00\x06\x6D" ++
                 "\x65\x6D\x62\x65\x72\x61\x02"
        test1 = TestCase $ assertEqual "function->binary"
            (binaryOk $ function1) binary
        test2 = TestCase $ assertEqual "binary->function"
            (termOk $ binary) function1
    in
    TestLabel "testFunction" (TestList [test1, test2])

testReference :: Test
testReference = 
    let reference1 =
            makeReference 100
                "\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e\x6f\x68\x6f\x73\x74"
                "\x00\x00\x00\xaf\x00\x00\x00\x03\x00\x00\x00\x00" 0
        binary = "\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40" ++
                 "\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\xAF\x00\x00\x00" ++
                 "\x03\x00\x00\x00\x00"
        test1 = TestCase $ assertEqual "reference->binary"
            (binaryOk $ reference1) binary
        test2 = TestCase $ assertEqual "binary->reference"
            (termOk $ binary) reference1
    in
    TestLabel "testReference" (TestList [test1, test2])

testDecodeBasic :: Test
testDecodeBasic =
    let test1 = TestCase $ assertEqual "decode basic 1"
            (termError $ "") "ParseError \"null input\""
        test2 = TestCase $ assertEqual "decode basic 2"
            (termError $ "\x83") "ParseError \"null input\""
        test3 = TestCase $ assertEqual "decode basic 3"
            (termError $ "\x83z") "ParseError \"invalid tag\""
    in
    TestLabel "testDecodeBasic" (TestList [test1, test2, test3])

main :: IO Counts
main = do
    results <- runTestTT $ TestList [
          testPid
        , testFunction
        , testReference
        , testDecodeBasic]
    if (errors results + failures results == 0) then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
