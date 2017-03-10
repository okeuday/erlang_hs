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
    case Erlang.binaryToTerm $ LazyByteString.fromStrict $ bytes binary of
        Left err -> error $ show err
        Right term -> term

termError :: String -> String
termError binary =
    case Erlang.binaryToTerm $ LazyByteString.fromStrict $ bytes binary of
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
            (termError $ "")
            "ParseError \"null input\""
        test2 = TestCase $ assertEqual "decode basic 2"
            (termError $ "\x83")
            "ParseError \"null input\""
        test3 = TestCase $ assertEqual "decode basic 3"
            (termError $ "\x83\x7a")
            "ParseError \"invalid tag\""
    in
    TestLabel "testDecodeBasic" (TestList [test1, test2, test3])

testDecodeAtom :: Test
testDecodeAtom =
    let test1 = TestCase $ assertEqual "decode atom 1"
            (termError $ "\x83\x64")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode atom 2"
            (termError $ "\x83\x64\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode atom 3"
            (termError $ "\x83\x64\x00\x01")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode atom 4"
            (termOk $ "\x83\x64\x00\x00")
            (Erlang.OtpErlangAtom (bytes ""))
        test5 = TestCase $ assertEqual "decode atom 5"
            (termOk $ "\x83\x73\x00")
            (Erlang.OtpErlangAtom (bytes ""))
        test6 = TestCase $ assertEqual "decode atom 6"
            (termOk $ "\x83\x64\x00\x04\&test")
            (Erlang.OtpErlangAtom (bytes "test"))
        test7 = TestCase $ assertEqual "decode atom 7"
            (termOk $ "\x83\x73\x04\&test")
            (Erlang.OtpErlangAtom (bytes "test"))
    in
    TestLabel "testDecodeAtom"
        (TestList [test1, test2, test3, test4, test5, test6, test7])

testDecodePredefinedAtom :: Test
testDecodePredefinedAtom =
    let test1 = TestCase $ assertEqual "decode predefined atom 1"
            (termOk $ "\x83\x73\x04\&true")
            (Erlang.OtpErlangAtomBool (True))
        test2 = TestCase $ assertEqual "decode predefined atom 2"
            (termOk $ "\x83\x73\x05\&false")
            (Erlang.OtpErlangAtomBool (False))
        test3 = TestCase $ assertEqual "decode predefined atom 3"
            (termOk $ "\x83\x73\x09\&undefined")
            (Erlang.OtpErlangAtom (bytes "undefined"))
    in
    TestLabel "testDecodePredefinedAtom" (TestList [test1, test2, test3])

testDecodeEmptyList :: Test
testDecodeEmptyList =
    let test1 = TestCase $ assertEqual "decode empty list 1"
            (termOk $ "\x83\x6a")
            (Erlang.OtpErlangList ([]))
    in
    TestLabel "testDecodeEmptyList" (TestList [test1])

testDecodeStringList :: Test
testDecodeStringList =
    let test1 = TestCase $ assertEqual "decode string list 1"
            (termError $ "\x83\x6b")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode string list 2"
            (termError $ "\x83\x6b\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode string list 3"
            (termError $ "\x83\x6b\x00\x01")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode string list 4"
            (termOk $ "\x83\x6b\x00\x00")
            (Erlang.OtpErlangString (bytes ""))
        test5 = TestCase $ assertEqual "decode string list 5"
            (termOk $ "\x83\x6b\x00\x04\&test")
            (Erlang.OtpErlangString (bytes "test"))
    in
    TestLabel "testDecodeStringList"
        (TestList [test1, test2, test3, test4, test5])

testDecodeList :: Test
testDecodeList =
    let test1 = TestCase $ assertEqual "decode list 1"
            (termError $ "\x83\x6c")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode list 2"
            (termError $ "\x83\x6c\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode list 3"
            (termError $ "\x83\x6c\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode list 4"
            (termError $ "\x83\x6c\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode list 5"
            (termError $ "\x83\x6c\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test6 = TestCase $ assertEqual "decode list 6"
            (termOk $ "\x83\x6c\x00\x00\x00\x00\x6a")
            (Erlang.OtpErlangList ([]))
        test7 = TestCase $ assertEqual "decode list 7"
            (termOk $ "\x83\x6c\x00\x00\x00\x02\x6a\x6a\x6a")
            (Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([]), Erlang.OtpErlangList ([])]))
    in
    TestLabel "testDecodeList"
        (TestList [test1, test2, test3, test4, test5, test6, test7])

testDecodeImproperList :: Test
testDecodeImproperList =
    let test1 = TestCase $ assertEqual "decode improper list 1"
            (termError $ "\x83\x6c\x00\x00\x00\x00\x6b")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode improper list 2"
            (termOk $ "\x83\x6c\x00\x00\x00\x01\x6a\x64\x00\x04\&tail")
            (Erlang.OtpErlangListImproper ([
                Erlang.OtpErlangList ([]),
                Erlang.OtpErlangAtom (bytes "tail")]))
    in
    TestLabel "testDecodeImproperList" (TestList [test1, test2])

testDecodeSmallTuple :: Test
testDecodeSmallTuple =
    let test1 = TestCase $ assertEqual "decode small tuple 1"
            (termError $ "\x83\x68")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode small tuple 2"
            (termError $ "\x83\x68\x01")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode small tuple 3"
            (termOk $ "\x83\x68\x00")
            (Erlang.OtpErlangTuple ([]))
        test4 = TestCase $ assertEqual "decode small tuple 4"
            (termOk $ "\x83\x68\x02\x6a\x6a")
            (Erlang.OtpErlangTuple ([
                Erlang.OtpErlangList ([]),
                Erlang.OtpErlangList ([])]))
    in
    TestLabel "testDecodeSmallTuple" (TestList [test1, test2, test3, test4])

testDecodeLargeTuple :: Test
testDecodeLargeTuple =
    let test1 = TestCase $ assertEqual "decode large tuple 1"
            (termError $ "\x83\x69")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode large tuple 2"
            (termError $ "\x83\x69\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode large tuple 3"
            (termError $ "\x83\x69\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode large tuple 4"
            (termError $ "\x83\x69\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode large tuple 5"
            (termError $ "\x83\x69\x00\x00\x00\x01")
            "ParseError \"not enough bytes\""
        test6 = TestCase $ assertEqual "decode large tuple 6"
            (termOk $ "\x83\x69\x00\x00\x00\x00")
            (Erlang.OtpErlangTuple ([]))
        test7 = TestCase $ assertEqual "decode large tuple 7"
            (termOk $ "\x83\x69\x00\x00\x00\x02\x6a\x6a")
            (Erlang.OtpErlangTuple ([
                Erlang.OtpErlangList ([]),
                Erlang.OtpErlangList ([])]))
    in
    TestLabel "testDecodeSmallTuple"
        (TestList [test1, test2, test3, test4, test5, test6, test7])

testDecodeSmallInteger :: Test
testDecodeSmallInteger =
    let test1 = TestCase $ assertEqual "decode small integer 1"
            (termError $ "\x83\x61")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode small integer 2"
            (termOk $ "\x83\x61\x00")
            (Erlang.OtpErlangInteger (0))
        test3 = TestCase $ assertEqual "decode small integer 3"
            (termOk $ "\x83\x61\xff")
            (Erlang.OtpErlangInteger (255))
    in
    TestLabel "testDecodeSmallInteger" (TestList [test1, test2, test3])

testDecodeInteger :: Test
testDecodeInteger =
    let test1 = TestCase $ assertEqual "decode integer 1"
            (termError $ "\x83\x62")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode integer 2"
            (termError $ "\x83\x62\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode integer 3"
            (termError $ "\x83\x62\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode integer 4"
            (termError $ "\x83\x62\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode integer 5"
            (termOk $ "\x83\x62\x00\x00\x00\x00")
            (Erlang.OtpErlangInteger (0))
        test6 = TestCase $ assertEqual "decode integer 6"
            (termOk $ "\x83\x62\x7f\xff\xff\xff")
            (Erlang.OtpErlangInteger (2147483647))
        test7 = TestCase $ assertEqual "decode integer 7"
            (termOk $ "\x83\x62\x80\x00\x00\x00")
            (Erlang.OtpErlangInteger (-2147483648))
        test8 = TestCase $ assertEqual "decode integer 8"
            (termOk $ "\x83\x62\xff\xff\xff\xff")
            (Erlang.OtpErlangInteger (-1))
        test9 = TestCase $ assertEqual "decode integer 9"
            (termOk $ "\x83\x6e\x08\x00\x00\x00\x00\x00\x00\x00\x00\x40")
            (Erlang.OtpErlangIntegerBig (4611686018427387904))
    in
    TestLabel "testDecodeInteger"
        (TestList [test1, test2, test3, test4, test5, test6, test7,
            test8, test9])

testDecodeBinary :: Test
testDecodeBinary =
    let test1 = TestCase $ assertEqual "decode binary 1"
            (termError $ "\x83\x6d")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode binary 2"
            (termError $ "\x83\x6d\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode binary 3"
            (termError $ "\x83\x6d\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode binary 4"
            (termError $ "\x83\x6d\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode binary 5"
            (termError $ "\x83\x6d\x00\x00\x00\x01")
            "ParseError \"not enough bytes\""
        test6 = TestCase $ assertEqual "decode binary 6"
            (termOk $ "\x83\x6d\x00\x00\x00\x00")
            (Erlang.OtpErlangBinary (ByteString.empty))
        test7 = TestCase $ assertEqual "decode binary 7"
            (termOk $ "\x83\x6d\x00\x00\x00\x04\&data")
            (Erlang.OtpErlangBinary (bytes "data"))
    in
    TestLabel "testDecodeBinary"
        (TestList [test1, test2, test3, test4, test5, test6, test7])

testDecodeFloat :: Test
testDecodeFloat =
    let test1 = TestCase $ assertEqual "decode float 1"
            (termError $ "\x83\x46")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode float 2"
            (termError $ "\x83\x46\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode float 3"
            (termError $ "\x83\x46\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode float 4"
            (termError $ "\x83\x46\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode float 5"
            (termError $ "\x83\x46\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test6 = TestCase $ assertEqual "decode float 6"
            (termError $ "\x83\x46\x00\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test7 = TestCase $ assertEqual "decode float 7"
            (termError $ "\x83\x46\x00\x00\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test8 = TestCase $ assertEqual "decode float 8"
            (termError $ "\x83\x46\x00\x00\x00\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test9 = TestCase $ assertEqual "decode float 9"
            (termOk $ "\x83\x46\x00\x00\x00\x00\x00\x00\x00\x00")
            (Erlang.OtpErlangFloat (0.0))
        test10 = TestCase $ assertEqual "decode float 10"
            (termOk $ "\x83\x46\x3f\xf8\x00\x00\x00\x00\x00\x00")
            (Erlang.OtpErlangFloat (1.5))
    in
    TestLabel "testDecodeFloat"
        (TestList [test1, test2, test3, test4, test5, test6, test7,
            test8, test9, test10])

testDecodeSmallBigInteger :: Test
testDecodeSmallBigInteger =
    let test1 = TestCase $ assertEqual "decode small big integer 1"
            (termError $ "\x83\x6e")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode small big integer 2"
            (termError $ "\x83\x6e\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode small big integer 3"
            (termError $ "\x83\x6e\x01\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode small big integer 4"
            (termOk $ "\x83\x6e\x00\x00")
            (Erlang.OtpErlangIntegerBig (0))
        test5 = TestCase $ assertEqual "decode small big integer 5"
            (termOk $ "\x83\x6e\x06\x00\x01\x02\x03\x04\x05\x06")
            (Erlang.OtpErlangIntegerBig (6618611909121))
        test6 = TestCase $ assertEqual "decode small big integer 6"
            (termOk $ "\x83\x6e\x06\x01\x01\x02\x03\x04\x05\x06")
            (Erlang.OtpErlangIntegerBig (-6618611909121))
    in
    TestLabel "testDecodeSmallBigInteger"
        (TestList [test1, test2, test3, test4, test5, test6])

testDecodeLargeBigInteger :: Test
testDecodeLargeBigInteger =
    let test1 = TestCase $ assertEqual "decode large big integer 1"
            (termError $ "\x83\x6f")
            "ParseError \"not enough bytes\""
        test2 = TestCase $ assertEqual "decode large big integer 2"
            (termError $ "\x83\x6f\x00")
            "ParseError \"not enough bytes\""
        test3 = TestCase $ assertEqual "decode large big integer 3"
            (termError $ "\x83\x6f\x00\x00")
            "ParseError \"not enough bytes\""
        test4 = TestCase $ assertEqual "decode large big integer 4"
            (termError $ "\x83\x6f\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test5 = TestCase $ assertEqual "decode large big integer 5"
            (termError $ "\x83\x6f\x00\x00\x00\x00")
            "ParseError \"not enough bytes\""
        test6 = TestCase $ assertEqual "decode large big integer 6"
            (termError $ "\x83\x6f\x00\x00\x00\x01\x00")
            "ParseError \"not enough bytes\""
        test7 = TestCase $ assertEqual "decode large big integer 7"
            (termOk $ "\x83\x6f\x00\x00\x00\x00\x00")
            (Erlang.OtpErlangIntegerBig (0))
        test8 = TestCase $ assertEqual "decode large big integer 8"
            (termOk $ "\x83\x6f\x00\x00\x00\x06\x00\x01\x02\x03\x04\x05\x06")
            (Erlang.OtpErlangIntegerBig (6618611909121))
        test9 = TestCase $ assertEqual "decode large big integer 9"
            (termOk $ "\x83\x6f\x00\x00\x00\x06\x01\x01\x02\x03\x04\x05\x06")
            (Erlang.OtpErlangIntegerBig (-6618611909121))
    in
    TestLabel "testDecodeLargeBigInteger"
        (TestList [test1, test2, test3, test4, test5, test6, test7,
            test8, test9])

main :: IO Counts
main = do
    results <- runTestTT $ TestList [
          testPid
        , testFunction
        , testReference
        , testDecodeBasic
        , testDecodeAtom
        , testDecodePredefinedAtom
        , testDecodeEmptyList
        , testDecodeStringList
        , testDecodeList
        , testDecodeImproperList
        , testDecodeSmallTuple
        , testDecodeLargeTuple
        , testDecodeSmallInteger
        , testDecodeInteger
        , testDecodeBinary
        , testDecodeFloat
        , testDecodeSmallBigInteger
        , testDecodeLargeBigInteger]
    if (errors results + failures results == 0) then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
