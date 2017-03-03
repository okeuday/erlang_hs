{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  BSD LICENSE

  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
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

module Foreign.Erlang
    ( OtpErlangTerm(..)
    , Pid
    , Port
    , Reference
    , Function
    , binaryToTerm
    ) where

import Control.Monad
import Prelude hiding (id,length,tail)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as DataInt
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Codec.Compression.Zlib as Zlib
import qualified Foreign.Erlang.Pid as Pid
import qualified Foreign.Erlang.Port as Port
import qualified Foreign.Erlang.Reference as Reference
import qualified Foreign.Erlang.Function as Function
type ByteString = ByteString.ByteString
type Get = Get.Get
type Int32 = DataInt.Int32
type Map = Map.Map
type Word8 = Word.Word8
type Word16 = Word.Word16
type Word32 = Word.Word32
type Pid = Pid.Pid
type Port = Port.Port
type Reference = Reference.Reference
type Function = Function.Function

-- tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
tagVersion :: Word8
tagVersion = 131
tagCompressedZlib :: Word8
tagCompressedZlib = 80
tagNewFloatExt :: Word8
tagNewFloatExt = 70
tagBitBinaryExt :: Word8
tagBitBinaryExt = 77
tagAtomCacheRef :: Word8
tagAtomCacheRef = 78
tagSmallIntegerExt :: Word8
tagSmallIntegerExt = 97
tagIntegerExt :: Word8
tagIntegerExt = 98
tagFloatExt :: Word8
tagFloatExt = 99
tagAtomExt :: Word8
tagAtomExt = 100
tagReferenceExt :: Word8
tagReferenceExt = 101
tagPortExt :: Word8
tagPortExt = 102
tagPidExt :: Word8
tagPidExt = 103
tagSmallTupleExt :: Word8
tagSmallTupleExt = 104
tagLargeTupleExt :: Word8
tagLargeTupleExt = 105
tagNilExt :: Word8
tagNilExt = 106
tagStringExt :: Word8
tagStringExt = 107
tagListExt :: Word8
tagListExt = 108
tagBinaryExt :: Word8
tagBinaryExt = 109
tagSmallBigExt :: Word8
tagSmallBigExt = 110
tagLargeBigExt :: Word8
tagLargeBigExt = 111
tagNewFunExt :: Word8
tagNewFunExt = 112
tagExportExt :: Word8
tagExportExt = 113
tagNewReferenceExt :: Word8
tagNewReferenceExt = 114
tagSmallAtomExt :: Word8
tagSmallAtomExt = 115
tagMapExt :: Word8
tagMapExt = 116
tagFunExt :: Word8
tagFunExt = 117
tagAtomUtf8Ext :: Word8
tagAtomUtf8Ext = 118
tagSmallAtomUtf8Ext :: Word8
tagSmallAtomUtf8Ext = 119

{-
bufferSize :: Int
bufferSize = 65536
-}

data OtpErlangTerm =
      OtpErlangInteger Int
    | OtpErlangIntegerBig Integer
    | OtpErlangFloat Double
    | OtpErlangAtom ByteString
    | OtpErlangAtomUTF8 ByteString
    | OtpErlangAtomCacheRef Int
    | OtpErlangAtomBool Bool
    | OtpErlangString ByteString
    | OtpErlangBinary ByteString
    | OtpErlangBinaryBits (ByteString, Int)
    | OtpErlangList [OtpErlangTerm]
    | OtpErlangListImproper [OtpErlangTerm]
    | OtpErlangTuple [OtpErlangTerm]
    | OtpErlangMap (Map OtpErlangTerm OtpErlangTerm)
    | OtpErlangPid Pid
    | OtpErlangPort Port
    | OtpErlangReference Reference
    | OtpErlangFunction Function
    deriving (Ord, Eq, Show)

getUnsignedInt8 :: Word8 -> Int
getUnsignedInt8 value = fromIntegral value

getUnsignedInt16 :: Word16 -> Int
getUnsignedInt16 value = fromIntegral value

getUnsignedInt32 :: Word32 -> Int
getUnsignedInt32 value = fromIntegral value

getSignedInt32 :: Word32 -> Int
getSignedInt32 value = fromIntegral (fromIntegral value :: Int32)

getUnsignedInt8or32 :: Bool -> Get Int
getUnsignedInt8or32 True = do
    value <- Get.getWord8
    return $ getUnsignedInt8 value
getUnsignedInt8or32 False = do
    value <- Get.getWord32be
    return $ getUnsignedInt32 value

binaryToTerm :: (Monad m) => ByteString -> m OtpErlangTerm
binaryToTerm binary =
    let size = ByteString.length binary in
    if size <= 1 then
        fail $ parseError "null input"
    else if ByteString.head binary /= tagVersion then
        fail $ parseError "invalid version"
    else
        let b = LazyByteString.fromStrict (ByteString.tail binary) in
        return $ Get.runGet binaryToTerms b

binaryToTerms :: Get OtpErlangTerm
binaryToTerms = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagNewFloatExt -> do
            value <- Get.getDoublebe
            return $ OtpErlangFloat value
        | tag == tagBitBinaryExt -> do
            j <- Get.getWord32be
            bits <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt32 j
            return $ OtpErlangBinaryBits (value, getUnsignedInt8 bits)
        | tag == tagAtomCacheRef -> do
            value <- Get.getWord8
            return $ OtpErlangAtomCacheRef $ getUnsignedInt8 value
        | tag == tagSmallIntegerExt -> do
            value <- Get.getWord8
            return $ OtpErlangInteger $ getUnsignedInt8 value
        | tag == tagIntegerExt -> do
            value <- Get.getWord32be
            return $ OtpErlangInteger $ getSignedInt32 value
        | tag == tagFloatExt -> do
            str <- Get.getByteString 31
            let value = Char8.unpack $ Char8.takeWhile (\c -> c /= '\0') str
            return $ OtpErlangFloat (read value :: Double)
        | tag == tagAtomExt -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return $ OtpErlangAtom value
        | tag == tagReferenceExt || tag == tagPortExt -> do
            (nodeTag, node) <- binaryToAtom
            id <- Get.getByteString 4
            creation <- Get.getWord8
            if tag == tagReferenceExt then
                return $ OtpErlangReference $ Reference.Reference
                    nodeTag node id creation
            else if tag == tagPortExt then
                return $ OtpErlangPort $ Port.Port
                    nodeTag node id creation
            else
                fail $ parseError "invalid"
        | tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            id <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getWord8
            return $ OtpErlangPid $ Pid.Pid
                nodeTag node id serial creation
        | tag == tagSmallTupleExt || tag == tagLargeTupleExt -> do
            length <- getUnsignedInt8or32 $ tag == tagSmallTupleExt
            value <- binaryToTermSequence length
            return $ OtpErlangTuple value
        | tag == tagNilExt -> do
            return $ OtpErlangList []
        | tag == tagStringExt -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return $ OtpErlangString value
        | tag == tagListExt -> do
            length <- Get.getWord32be
            tmp <- binaryToTermSequence $ getUnsignedInt32 length
            tail <- binaryToTerms
            if tail == OtpErlangList [] then
                return $ OtpErlangList tmp
            else
                return $ OtpErlangListImproper $ tmp ++ [tail]
        | tag == tagBinaryExt -> do
            j <- Get.getWord32be
            value <- Get.getByteString $ getUnsignedInt32 j
            return $ OtpErlangBinary value
        | tag == tagSmallBigExt || tag == tagLargeBigExt -> do
            j <- getUnsignedInt8or32 (tag == tagSmallBigExt)
            sign <- Get.getWord8
            digits <- replicateM j Get.getWord8
            let f = (\d -> \b -> b * 256 + (fromIntegral . getUnsignedInt8) d)
                value = List.foldr f (0 :: Integer) digits
            if sign == 1 then
                return $ OtpErlangIntegerBig $ (-1) * value
            else
                return $ OtpErlangIntegerBig value
        | tag == tagNewFunExt -> do
            length <- Get.getWord32be
            value <- Get.getByteString $ getUnsignedInt32 length
            return $ OtpErlangFunction $ Function.Function
                tag value
        | tag == tagExportExt -> do
            length <- Get.lookAhead $ binaryToExportSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ Function.Function
                tag value
        | tag == tagNewReferenceExt -> do
            j <- Get.getWord16be
            (nodeTag, node) <- binaryToAtom
            creation <- Get.getWord8
            id <- Get.getByteString $ (getUnsignedInt16 j) * 4
            return $ OtpErlangReference $ Reference.Reference
                nodeTag node id creation
        | tag == tagSmallAtomExt -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            return $ OtpErlangAtom value
        | tag == tagMapExt -> do
            length <- Get.getWord32be
            pairs <- replicateM (getUnsignedInt32 length) binaryToPair
            return $ OtpErlangMap $ Map.fromList pairs
        | tag == tagFunExt -> do
            length <- Get.lookAhead $ binaryToFunSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ Function.Function
                tag value
        | tag == tagAtomUtf8Ext -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return $ OtpErlangAtomUTF8 value
        | tag == tagSmallAtomUtf8Ext -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            return $ OtpErlangAtomUTF8 value
        | tag == tagCompressedZlib -> do
            size_uncompressed <- Get.getWord32be
            compressed <- Get.getRemainingLazyByteString
            let data_uncompressed = Zlib.decompress $ compressed
                size1 = fromIntegral $ getUnsignedInt32 size_uncompressed
                size2 = LazyByteString.length data_uncompressed
            if size1 /= size2 then
                fail $ parseError "compression corrupt"
            else
                return $ Get.runGet binaryToTerms data_uncompressed
        | otherwise ->
            fail $ parseError "invalid tag"

binaryToTermSequence :: Int -> Get [OtpErlangTerm]
binaryToTermSequence length = do
    value <- replicateM length binaryToTerms
    return value

binaryToPair :: Get (OtpErlangTerm, OtpErlangTerm)
binaryToPair = do
    key <- binaryToTerms
    value <- binaryToTerms
    return (key, value)

binaryToExportSize :: Get Int
binaryToExportSize = do
    old_i <- Get.bytesRead
    (_, _) <- binaryToAtom -- module
    (_, _) <- binaryToAtom -- function
    arityTag <- Get.getWord8
    _ <- Get.getWord8 -- arity
    i <- Get.bytesRead
    if arityTag == tagSmallIntegerExt then
        return $ fromIntegral (i - old_i)
    else
        fail $ parseError "invalid small integer tag"

binaryToFunSize :: Get Int
binaryToFunSize = do
    old_i <- Get.bytesRead
    numfree <- Get.getWord32be
    _ <- binaryToPid -- pid
    (_, _) <- binaryToAtom -- module
    _ <- binaryToInteger -- index
    _ <- binaryToInteger -- uniq
    _ <- binaryToTermSequence (getUnsignedInt32 numfree) -- free
    i <- Get.bytesRead
    return $ fromIntegral (i - old_i)

binaryToInteger :: Get Int
binaryToInteger = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagSmallIntegerExt -> do
            value <- Get.getWord8
            return $ getUnsignedInt8 value
        | tag == tagIntegerExt -> do
            value <- Get.getWord32be
            return $ getSignedInt32 value
        | otherwise ->
            fail $ parseError "invalid integer tag"

binaryToPid :: Get Pid
binaryToPid = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            id <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getWord8
            return $ Pid.Pid
                nodeTag node id serial creation
        | otherwise ->
            fail $ parseError "invalid pid tag"

binaryToAtom :: Get (Word8, ByteString)
binaryToAtom = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagAtomExt -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return (tag, value)
        | tag == tagAtomCacheRef -> do
            value <- Get.getByteString 1
            return (tag, value)
        | tag == tagSmallAtomExt -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            return (tag, value)
        | tag == tagAtomUtf8Ext -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return (tag, value)
        | tag == tagSmallAtomUtf8Ext -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            return (tag, value)
        | otherwise ->
            fail $ parseError "invalid atom tag"

{-
outputError :: String -> String
outputError s =
    "outputError: " ++ s
-}
parseError :: String -> String
parseError s =
    "parseError: " ++ s
