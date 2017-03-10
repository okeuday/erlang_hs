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

duplicate :: Int -> String -> String
duplicate n str = [1..n] >> str

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

testEncodeTuple :: Test
testEncodeTuple =
    let test1 = TestCase $ assertEqual "encode tuple 1"
            (binaryOk $ Erlang.OtpErlangTuple ([]))
            "\x83\x68\x00"
        test2 = TestCase $ assertEqual "encode tuple 2"
            (binaryOk $ Erlang.OtpErlangTuple ([
                Erlang.OtpErlangTuple ([]), Erlang.OtpErlangTuple ([])]))
            "\x83\x68\x02\x68\x00\x68\x00"
        tupleCreate size l =
            if size == 0 then
                Erlang.OtpErlangTuple (l)
            else
                tupleCreate (size - 1 :: Int) ((Erlang.OtpErlangTuple ([])):l)
        test3 = TestCase $ assertEqual "encode tuple 3"
            (binaryOk $ tupleCreate 255 [])
            ("\x83\x68\xff" ++ (duplicate 255 "\x68\x00"))
        test4 = TestCase $ assertEqual "encode tuple 4"
            (binaryOk $ tupleCreate 256 [])
            ("\x83\x69\x00\x00\x01\x00" ++ (duplicate 256 "\x68\x00"))
    in
    TestLabel "testEncodeTuple" (TestList [test1, test2, test3, test4])

testEncodeEmptyList :: Test
testEncodeEmptyList =
    let test1 = TestCase $ assertEqual "encode empty list 1"
            (binaryOk $ Erlang.OtpErlangList ([]))
            "\x83\x6a"
    in
    TestLabel "testEncodeEmptyList" (TestList [test1])

testEncodeStringList :: Test
testEncodeStringList =
    let test1 = TestCase $ assertEqual "encode string list 1"
            (binaryOk $ Erlang.OtpErlangString (bytes ""))
            "\x83\x6a"
        test2 = TestCase $ assertEqual "encode string list 2"
            (binaryOk $ Erlang.OtpErlangString (bytes "\x00"))
            "\x83\x6b\x00\x01\x00"
        s = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r" ++
            "\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a" ++
            "\x1b\x1c\x1d\x1e\x1f\& !\"#$%&'()*+,-./0123456789:;<=>" ++
            "?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopq" ++
            "rstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88" ++
            "\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95" ++
            "\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2" ++
            "\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf" ++
            "\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc" ++
            "\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9" ++
            "\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6" ++
            "\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3" ++
            "\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0" ++
            "\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"
        test3 = TestCase $ assertEqual "encode string list 3"
            (binaryOk $ Erlang.OtpErlangString (bytes s))
            ("\x83\x6b\x01\x00" ++ s)
    in
    TestLabel "testEncodeStringList" (TestList [test1, test2, test3])

testEncodeListBasic :: Test
testEncodeListBasic =
    let test1 = TestCase $ assertEqual "encode list basic 1"
            (binaryOk $ Erlang.OtpErlangString (bytes ""))
            "\x83\x6A"
        test2 = TestCase $ assertEqual "encode list basic 2"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangString (bytes "")]))
            "\x83\x6C\x00\x00\x00\x01\x6A\x6A"
        test3 = TestCase $ assertEqual "encode list basic 3"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (1)]))
            "\x83\x6C\x00\x00\x00\x01\x61\x01\x6A"
        test4 = TestCase $ assertEqual "encode list basic 4"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (255)]))
            "\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A"
        test5 = TestCase $ assertEqual "encode list basic 5"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (256)]))
            "\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A"
        test6 = TestCase $ assertEqual "encode list basic 6"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (2147483647)]))
            "\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A"
        test7 = TestCase $ assertEqual "encode list basic 7"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangIntegerBig (2147483648)]))
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A"
        test8 = TestCase $ assertEqual "encode list basic 8"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (0)]))
            "\x83\x6C\x00\x00\x00\x01\x61\x00\x6A"
        test9 = TestCase $ assertEqual "encode list basic 9"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (-1)]))
            "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A"
        test10 = TestCase $ assertEqual "encode list basic 10"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (-256)]))
            "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A"
        test11 = TestCase $ assertEqual "encode list basic 11"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (-257)]))
            "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A"
        test12 = TestCase $ assertEqual "encode list basic 12"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (-2147483648)]))
            "\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A"
        test13 = TestCase $ assertEqual "encode list basic 13"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (-2147483649)]))
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A"
        test14 = TestCase $ assertEqual "encode list basic 14"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangString (bytes "test")]))
            "\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A"
        test15 = TestCase $ assertEqual "encode list basic 15"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangInteger (373),
                Erlang.OtpErlangInteger (455)]))
            ("\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00" ++
             "\x01\xC7\x6A")
        test16 = TestCase $ assertEqual "encode list basic 16"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([])]))
            "\x83\x6C\x00\x00\x00\x01\x6A\x6A"
        test17 = TestCase $ assertEqual "encode list basic 17"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([]), Erlang.OtpErlangList ([])]))
            "\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A"
        test18 = TestCase $ assertEqual "encode list basic 18"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([
                    Erlang.OtpErlangString (bytes "this"),
                    Erlang.OtpErlangString (bytes "is")
                ]),
                Erlang.OtpErlangList ([Erlang.OtpErlangList ([
                    Erlang.OtpErlangString (bytes "a")])]),
                Erlang.OtpErlangString (bytes "test")
                ]))
            ("\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04" ++
             "\x74\x68\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00" ++
             "\x01\x6C\x00\x00\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00" ++
             "\x04\x74\x65\x73\x74\x6A")
    in
    TestLabel "testEncodeListBasic"
        (TestList [test1, test2, test3, test4, test5, test6, test7,
            test8, test9, test10, test11, test12, test13, test14,
            test15, test16, test17, test18])

testEncodeList :: Test
testEncodeList =
    let test1 = TestCase $ assertEqual "encode list 1"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([])]))
            "\x83\x6c\x00\x00\x00\x01\x6a\x6a"
        test2 = TestCase $ assertEqual "encode list 2"
            (binaryOk $ Erlang.OtpErlangList ([
                Erlang.OtpErlangList ([]), Erlang.OtpErlangList ([]),
                Erlang.OtpErlangList ([]), Erlang.OtpErlangList ([]),
                Erlang.OtpErlangList ([])]))
            "\x83\x6c\x00\x00\x00\x05\x6a\x6a\x6a\x6a\x6a\x6a"
    in
    TestLabel "testEncodeList" (TestList [test1, test2])

testEncodeImproperList :: Test
testEncodeImproperList =
    let test1 = TestCase $ assertEqual "encode improper list 1"
            (binaryOk $ Erlang.OtpErlangListImproper ([
                Erlang.OtpErlangTuple ([]), Erlang.OtpErlangTuple ([])]))
            "\x83\x6c\x00\x00\x00\x01\x68\x00\x68\x00"
        test2 = TestCase $ assertEqual "encode improper list 2"
            (binaryOk $ Erlang.OtpErlangListImproper ([
                Erlang.OtpErlangInteger (0), Erlang.OtpErlangInteger (1)]))
            "\x83\x6c\x00\x00\x00\x01\x61\x00\x61\x01"
    in
    TestLabel "testEncodeImproperList" (TestList [test1, test2])

testEncodeUnicode :: Test
testEncodeUnicode =
    let test1 = TestCase $ assertEqual "encode unicode 1"
            (binaryOk $ Erlang.OtpErlangString (bytes ""))
            "\x83\x6a"
        test2 = TestCase $ assertEqual "encode unicode 2"
            (binaryOk $ Erlang.OtpErlangString (bytes "test"))
            "\x83\x6b\x00\x04\&test"
        test3 = TestCase $ assertEqual "encode unicode 3"
            (binaryOk $ Erlang.OtpErlangString (bytes "\x00\xc3\xbf"))
            "\x83\x6b\x00\x03\x00\xc3\xbf"
        test4 = TestCase $ assertEqual "encode unicode 4"
            (binaryOk $ Erlang.OtpErlangString (bytes "\xc4\x80"))
            "\x83\x6b\x00\x02\xc4\x80"
        test5 = TestCase $ assertEqual "encode unicode 5"
            (binaryOk $ Erlang.OtpErlangString
                (bytes "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82"))
            "\x83\x6b\x00\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82"
        test6 = TestCase $ assertEqual "encode unicode 6"
            (binaryOk $ Erlang.OtpErlangString
                (bytes $ duplicate 65536 "\xd0\x90"))
            ("\x83\x6c\x00\x02\x00\x00" ++
                (duplicate 65536 "\x61\xd0\x61\x90") ++ "\x6a")
    in
    TestLabel "testEncodeUnicode"
        (TestList [test1, test2, test3, test4, test5, test6])

testEncodeAtom :: Test
testEncodeAtom =
    let test1 = TestCase $ assertEqual "encode atom 1"
            (binaryOk $ Erlang.OtpErlangAtom (bytes ""))
            "\x83\x73\x00"
        test2 = TestCase $ assertEqual "encode atom 2"
            (binaryOk $ Erlang.OtpErlangAtom (bytes "test"))
            "\x83\x73\x04\&test"
    in
    TestLabel "testEncodeAtom" (TestList [test1, test2])

testEncodeStringBasic :: Test
testEncodeStringBasic =
    let test1 = TestCase $ assertEqual "encode string basic 1"
            (binaryOk $ Erlang.OtpErlangString (bytes ""))
            "\x83\x6A"
        test2 = TestCase $ assertEqual "encode string basic 2"
            (binaryOk $ Erlang.OtpErlangString (bytes "test"))
            "\x83\x6B\x00\x04\x74\x65\x73\x74"
        test3 = TestCase $ assertEqual "encode string basic 3"
            (binaryOk $ Erlang.OtpErlangString (bytes "two words"))
            "\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73"
        test4 = TestCase $ assertEqual "encode string basic 4"
            (binaryOk $ Erlang.OtpErlangString
                (bytes "testing multiple words"))
            ("\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D" ++
             "\x75\x6C\x74\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73")
        test5 = TestCase $ assertEqual "encode string basic 5"
            (binaryOk $ Erlang.OtpErlangString (bytes " "))
            "\x83\x6B\x00\x01\x20"
        test6 = TestCase $ assertEqual "encode string basic 6"
            (binaryOk $ Erlang.OtpErlangString (bytes "  "))
            "\x83\x6B\x00\x02\x20\x20"
        test7 = TestCase $ assertEqual "encode string basic 7"
            (binaryOk $ Erlang.OtpErlangString (bytes "1"))
            "\x83\x6B\x00\x01\x31"
        test8 = TestCase $ assertEqual "encode string basic 8"
            (binaryOk $ Erlang.OtpErlangString (bytes "37"))
            "\x83\x6B\x00\x02\x33\x37"
        test9 = TestCase $ assertEqual "encode string basic 9"
            (binaryOk $ Erlang.OtpErlangString (bytes "one = 1"))
            "\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31"
        test10 = TestCase $ assertEqual "encode string basic 10"
            (binaryOk $ Erlang.OtpErlangString
                (bytes "!@#$%^&*()_+-=[]{}\\|;':\",./<>?~`"))
            ("\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28" ++
             "\x29\x5F\x2B\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27" ++
             "\x3A\x22\x2C\x2E\x2F\x3C\x3E\x3F\x7E\x60")
        test11 = TestCase $ assertEqual "encode string basic 11"
            (binaryOk $ Erlang.OtpErlangString
                (bytes "\"\x08\x0c\n\r\t\x0b\&S\x12"))
            "\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12"
    in
    TestLabel "testEncodeStringBasic"
        (TestList [test1, test2, test3, test4, test5, test6, test7,
            test8, test9, test10, test11])

testEncodeString :: Test
testEncodeString =
    let test1 = TestCase $ assertEqual "encode string 1"
            (binaryOk $ Erlang.OtpErlangString (bytes ""))
            "\x83\x6A"
        test2 = TestCase $ assertEqual "encode string 2"
            (binaryOk $ Erlang.OtpErlangString (bytes "test"))
            "\x83\x6B\x00\x04\&test"
    in
    TestLabel "testEncodeString" (TestList [test1, test2])

testEncodeBoolean :: Test
testEncodeBoolean =
    let test1 = TestCase $ assertEqual "encode boolean 1"
            (binaryOk $ Erlang.OtpErlangAtomBool (True))
            "\x83\x73\x04\&true"
        test2 = TestCase $ assertEqual "encode boolean 2"
            (binaryOk $ Erlang.OtpErlangAtomBool (False))
            "\x83\x73\x05\&false"
    in
    TestLabel "testEncodeBoolean" (TestList [test1, test2])

testEncodeSmallInteger :: Test
testEncodeSmallInteger =
    let test1 = TestCase $ assertEqual "encode small integer 1"
            (binaryOk $ Erlang.OtpErlangInteger (0))
            "\x83\x61\x00"
        test2 = TestCase $ assertEqual "encode small integer 2"
            (binaryOk $ Erlang.OtpErlangInteger (255))
            "\x83\x61\xff"
    in
    TestLabel "testEncodeSmallInteger" (TestList [test1, test2])

testEncodeInteger :: Test
testEncodeInteger =
    let test1 = TestCase $ assertEqual "encode integer 1"
            (binaryOk $ Erlang.OtpErlangInteger (-1))
            "\x83\x62\xff\xff\xff\xff"
        test2 = TestCase $ assertEqual "encode integer 2"
            (binaryOk $ Erlang.OtpErlangInteger (-2147483648))
            "\x83\x62\x80\x00\x00\x00"
        test3 = TestCase $ assertEqual "encode integer 3"
            (binaryOk $ Erlang.OtpErlangInteger (256))
            "\x83\x62\x00\x00\x01\x00"
        test4 = TestCase $ assertEqual "encode integer 4"
            (binaryOk $ Erlang.OtpErlangInteger (2147483647))
            "\x83\x62\x7f\xff\xff\xff"
    in
    TestLabel "testEncodeInteger" (TestList [test1, test2, test3, test4])

testEncodeSmallBigInteger :: Test
testEncodeSmallBigInteger =
    let test1 = TestCase $ assertEqual "encode small big integer 1"
            (binaryOk $ Erlang.OtpErlangIntegerBig (2147483648))
            "\x83\x6e\x04\x00\x00\x00\x00\x80"
        test2 = TestCase $ assertEqual "encode small big integer 2"
            (binaryOk $ Erlang.OtpErlangIntegerBig (-2147483649))
            "\x83\x6e\x04\x01\x01\x00\x00\x80"
    in
    TestLabel "testEncodeSmallBigInteger" (TestList [test1, test2])

testEncodeLargeBigInteger :: Test
testEncodeLargeBigInteger =
    let test1 = TestCase $ assertEqual "encode large big integer 1"
            (binaryOk $ Erlang.OtpErlangIntegerBig (126238304966058622268417487065116999845484776053576109500509161826268184136202698801551568013761380717534054534851164138648904527931605160527688095259563605939964364716019515983399209962459578542172100149937763938581219604072733422507180056009672540900709554109516816573779593326332288314873251559077853068444977864803391962580800682760017849589281937637993445539366428356761821065267423102149447628375691862210717202025241630303118559188678304314076943801692528246980959705901641444238894928620825482303431806955690226308773426829503900930529395181208739591967195841536053143145775307050594328881077553168201547776))
            ("\x83\x6f\x00\x00\x01\x00\x00" ++
             (duplicate 255 "\x00") ++ "\x01")
        test2 = TestCase $ assertEqual "encode large big integer 2"
            (binaryOk $ Erlang.OtpErlangIntegerBig (-126238304966058622268417487065116999845484776053576109500509161826268184136202698801551568013761380717534054534851164138648904527931605160527688095259563605939964364716019515983399209962459578542172100149937763938581219604072733422507180056009672540900709554109516816573779593326332288314873251559077853068444977864803391962580800682760017849589281937637993445539366428356761821065267423102149447628375691862210717202025241630303118559188678304314076943801692528246980959705901641444238894928620825482303431806955690226308773426829503900930529395181208739591967195841536053143145775307050594328881077553168201547776))
            ("\x83\x6f\x00\x00\x01\x00\x01" ++
             (duplicate 255 "\x00") ++ "\x01")
    in
    TestLabel "testEncodeLargeBigInteger" (TestList [test1, test2])

testEncodeFloat :: Test
testEncodeFloat =
    let test1 = TestCase $ assertEqual "encode float 1"
            (binaryOk $ Erlang.OtpErlangFloat (0.0))
            "\x83\x46\x00\x00\x00\x00\x00\x00\x00\x00"
        test2 = TestCase $ assertEqual "encode float 2"
            (binaryOk $ Erlang.OtpErlangFloat (0.5))
            "\x83\x46\x3f\xe0\x00\x00\x00\x00\x00\x00"
        test3 = TestCase $ assertEqual "encode float 3"
            (binaryOk $ Erlang.OtpErlangFloat (-0.5))
            "\x83\x46\xbf\xe0\x00\x00\x00\x00\x00\x00"
        test4 = TestCase $ assertEqual "encode float 4"
            (binaryOk $ Erlang.OtpErlangFloat (3.1415926))
            "\x83\x46\x40\x09\x21\xfb\x4d\x12\xd8\x4a"
        test5 = TestCase $ assertEqual "encode float 5"
            (binaryOk $ Erlang.OtpErlangFloat (-3.1415926))
            "\x83\x46\xc0\x09\x21\xfb\x4d\x12\xd8\x4a"
    in
    TestLabel "testEncodeFloat" (TestList [test1, test2, test3, test4, test5])

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
        , testDecodeLargeBigInteger
        , testEncodeTuple
        , testEncodeEmptyList
        , testEncodeStringList
        , testEncodeListBasic
        , testEncodeList
        , testEncodeImproperList
        , testEncodeUnicode
        , testEncodeAtom
        , testEncodeStringBasic
        , testEncodeString
        , testEncodeBoolean
        , testEncodeSmallInteger
        , testEncodeInteger
        , testEncodeSmallBigInteger
        , testEncodeLargeBigInteger
        , testEncodeFloat]
    if (errors results + failures results == 0) then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
