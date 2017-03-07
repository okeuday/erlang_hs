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
    , Error(..)
    , Result
    , binaryToTerm
    , termToBinary
    ) where

import Control.Monad
import Control.Applicative
import Prelude hiding (length,tail)
import Data.Bits ((.&.))
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as DataInt
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Word as Word
import qualified Codec.Compression.Zlib as Zlib
import qualified Foreign.Erlang.Pid as E
import qualified Foreign.Erlang.Port as E
import qualified Foreign.Erlang.Reference as E
import qualified Foreign.Erlang.Function as E
type Get = Get.Get
type Put = Put.Put
--newtype EPutM e a = EPutM { _runEPutM :: Either e (Put.PutM a) }
type Builder = Builder.Builder
type ByteString = ByteString.ByteString
type LazyByteString = LazyByteString.ByteString
type Int32 = DataInt.Int32
type Map = Map.Map
type Word8 = Word.Word8
type Word16 = Word.Word16
type Word32 = Word.Word32
type Pid = E.Pid
type Port = E.Port
type Reference = E.Reference
type Function = E.Function

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

data Error =
      InputError String
    | OutputError String
    | ParseError String
    deriving (Eq, Show)

type Result a = Either Error a

ok :: a -> Result a
ok value = Right value

errorType :: Error -> Result a
errorType value = Left value

newtype PutMResult a =
    PutMResult { getPutResult :: Result (Put.PutM a) }
type PutResult = PutMResult ()
instance Monad PutMResult where
    PutMResult (Left  l) >>= _ = PutMResult (Left l)
    PutMResult (Right r) >>= k = PutMResult (k r)

liftPut :: (a -> Put) -> (a -> PutResult)
liftPut f = PutMResult . pure . f

putErrorType :: Error -> PutResult
putErrorType value = PutMResult $ Left value

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

binaryToTerm :: LazyByteString -> Result OtpErlangTerm
binaryToTerm binary =
    let size = LazyByteString.length binary in
    if size <= 1 then
        errorType $ ParseError "null input"
    else if LazyByteString.head binary /= tagVersion then
        errorType $ ParseError "invalid version"
    else
        case Get.runGetOrFail binaryToTerms (LazyByteString.tail binary) of
          Left (_, _, err) -> errorType $ ParseError err
          Right (_, _, term) -> ok term

termToBinary :: OtpErlangTerm -> Int -> Result LazyByteString
termToBinary term compressed
    | compressed < (-1) || compressed > 9 =
        errorType $ InputError "compressed in [-1..9]"
    | otherwise =
        case termsToBinary term of
            PutMResult (Left err) ->
                errorType err
            PutMResult (Right put) ->
                let (_, dataUncompressed) = Put.runPutM put in
                if compressed == (-1) then
                    ok $ LazyByteString.cons tagVersion dataUncompressed
                else
                    let sizeUncompressed =
                            LazyByteString.length dataUncompressed
                        params = Zlib.defaultCompressParams {
                            Zlib.compressLevel =
                            Zlib.CompressionLevel compressed }
                        dataCompressed =
                            Zlib.compressWith params dataUncompressed in
                    if sizeUncompressed > 4294967295 then
                        errorType $ OutputError "uint32 overflow"
                    else
                        ok $ Put.runPut $ Put.putBuilder $ Builder.append
                            (Builder.append (Builder.singleton tagVersion)
                                (Builder.putWord32be $
                                    fromIntegral sizeUncompressed))
                            (Builder.fromLazyByteString dataCompressed)

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
            eid <- Get.getByteString 4
            creation <- Get.getWord8
            if tag == tagReferenceExt then
                return $ OtpErlangReference $ E.Reference
                    nodeTag node eid creation
            else if tag == tagPortExt then
                return $ OtpErlangPort $ E.Port
                    nodeTag node eid creation
            else
                fail $ "invalid"
        | tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            eid <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getWord8
            return $ OtpErlangPid $ E.Pid
                nodeTag node eid serial creation
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
            return $ OtpErlangFunction $ E.Function
                tag value
        | tag == tagExportExt -> do
            length <- Get.lookAhead $ binaryToExportSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ E.Function
                tag value
        | tag == tagNewReferenceExt -> do
            j <- Get.getWord16be
            (nodeTag, node) <- binaryToAtom
            creation <- Get.getWord8
            eid <- Get.getByteString $ (getUnsignedInt16 j) * 4
            return $ OtpErlangReference $ E.Reference
                nodeTag node eid creation
        | tag == tagSmallAtomExt -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            return $ OtpErlangAtom value
        | tag == tagMapExt -> do
            length <- Get.getWord32be
            pairs <- replicateM (getUnsignedInt32 length) binaryToMapPair
            return $ OtpErlangMap $ Map.fromList pairs
        | tag == tagFunExt -> do
            length <- Get.lookAhead $ binaryToFunSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ E.Function
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
            sizeUncompressed <- Get.getWord32be
            compressed <- Get.getRemainingLazyByteString
            let dataUncompressed = Zlib.decompress $ compressed
                size1 = fromIntegral $ getUnsignedInt32 sizeUncompressed
                size2 = LazyByteString.length dataUncompressed
            if size1 /= size2 then
                fail $ "compression corrupt"
            else
                return $ Get.runGet binaryToTerms dataUncompressed
        | otherwise ->
            fail $ "invalid tag"

binaryToTermSequence :: Int -> Get [OtpErlangTerm]
binaryToTermSequence length = do
    value <- replicateM length binaryToTerms
    return value

binaryToMapPair :: Get (OtpErlangTerm, OtpErlangTerm)
binaryToMapPair = do
    key <- binaryToTerms
    value <- binaryToTerms
    return (key, value)

binaryToExportSize :: Get Int
binaryToExportSize = do
    oldI <- Get.bytesRead
    (_, _) <- binaryToAtom -- module
    (_, _) <- binaryToAtom -- function
    arityTag <- Get.getWord8
    _ <- Get.getWord8 -- arity
    i <- Get.bytesRead
    if arityTag == tagSmallIntegerExt then
        return $ fromIntegral (i - oldI)
    else
        fail $ "invalid small integer tag"

binaryToFunSize :: Get Int
binaryToFunSize = do
    oldI <- Get.bytesRead
    numfree <- Get.getWord32be
    _ <- binaryToPid -- pid
    (_, _) <- binaryToAtom -- module
    _ <- binaryToInteger -- index
    _ <- binaryToInteger -- uniq
    _ <- binaryToTermSequence (getUnsignedInt32 numfree) -- free
    i <- Get.bytesRead
    return $ fromIntegral (i - oldI)

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
            fail $ "invalid integer tag"

binaryToPid :: Get Pid
binaryToPid = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            eid <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getWord8
            return $ E.Pid
                nodeTag node eid serial creation
        | otherwise ->
            fail $ "invalid pid tag"

binaryToAtom :: Get (Word8, ByteString)
binaryToAtom = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagAtomExt -> do
            j <- Get.lookAhead $ Get.getWord16be
            value <- Get.getByteString $ 2 + (getUnsignedInt16 j)
            return (tag, value)
        | tag == tagAtomCacheRef -> do
            value <- Get.getByteString 1
            return (tag, value)
        | tag == tagSmallAtomExt -> do
            j <- Get.lookAhead $ Get.getWord8
            value <- Get.getByteString $ 1 + (getUnsignedInt8 j)
            return (tag, value)
        | tag == tagAtomUtf8Ext -> do
            j <- Get.lookAhead $ Get.getWord16be
            value <- Get.getByteString $ 2 + (getUnsignedInt16 j)
            return (tag, value)
        | tag == tagSmallAtomUtf8Ext -> do
            j <- Get.lookAhead $ Get.getWord8
            value <- Get.getByteString $ 1 + (getUnsignedInt8 j)
            return (tag, value)
        | otherwise ->
            fail $ "invalid atom tag"

termsToBinary :: OtpErlangTerm -> PutResult
termsToBinary (OtpErlangInteger value)
    | value >= 0 && value <= 255 = do
        (liftPut Put.putWord8) tagSmallIntegerExt
        (liftPut Put.putWord8) $ fromIntegral value
    | value >= (-2147483648) && value <= 2147483647 = do
        (liftPut Put.putWord8) tagIntegerExt
        (liftPut Put.putInt32be) $ fromIntegral value
    | otherwise =
        termsToBinary $ OtpErlangIntegerBig $ fromIntegral value
termsToBinary (OtpErlangIntegerBig value) =
    let sign = if value < 0 then 1 else 0
        loop bignum l =
            if bignum > 0 then
                loop (bignum `quot` 256)
                    (LazyByteString.cons (fromIntegral $ bignum .&. 255) l)
            else
                LazyByteString.reverse l
        lResult = loop (abs value) LazyByteString.empty
        lLength = LazyByteString.length lResult in
    if lLength <= 255 then do
        (liftPut Put.putWord8) tagSmallBigExt
        (liftPut Put.putWord8) $ fromIntegral lLength
        (liftPut Put.putWord8) sign
        (liftPut Put.putBuilder) $ Builder.fromLazyByteString lResult
    else if lLength <= 4294967295 then do
        (liftPut Put.putWord8) tagLargeBigExt
        (liftPut Put.putWord32be) $ fromIntegral lLength
        (liftPut Put.putWord8) sign
        (liftPut Put.putBuilder) $ Builder.fromLazyByteString lResult
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangFloat value) = do
    (liftPut Put.putWord8) tagNewFloatExt
    (liftPut Put.putDoublebe) value
termsToBinary (OtpErlangAtom value) =
    let length = ByteString.length value in
    if length <= 255 then do
        (liftPut Put.putWord8) tagSmallAtomExt
        (liftPut Put.putWord8) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else if length <= 65535 then do
        (liftPut Put.putWord8) tagAtomExt
        (liftPut Put.putWord16be) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else
        putErrorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangAtomUTF8 value) =
    let length = ByteString.length value in
    if length <= 255 then do
        (liftPut Put.putWord8) tagSmallAtomUtf8Ext
        (liftPut Put.putWord8) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else if length <= 65535 then do
        (liftPut Put.putWord8) tagAtomUtf8Ext
        (liftPut Put.putWord16be) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else
        putErrorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangAtomCacheRef value) = do
    (liftPut Put.putWord8) tagAtomCacheRef
    (liftPut Put.putWord8) $ fromIntegral value
termsToBinary (OtpErlangAtomBool value) =
    if value then
        termsToBinary $ OtpErlangAtom $ Char8.pack "true"
    else
        termsToBinary $ OtpErlangAtom $ Char8.pack "false"
termsToBinary (OtpErlangString value) =
    let length = ByteString.length value in
    if length == 0 then do
        (liftPut Put.putWord8) tagNilExt
    else if length <= 65535 then do
        (liftPut Put.putWord8) tagStringExt
        (liftPut Put.putWord16be) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else if length <= 4294967295 then do
        (liftPut Put.putWord8) tagListExt
        (liftPut Put.putWord32be) $ fromIntegral length
        (liftPut Put.putWord8) tagSmallIntegerExt
        (liftPut Put.putBuilder) $ Builder.fromByteString $
            ByteString.intersperse tagSmallIntegerExt value
        (liftPut Put.putWord8) tagNilExt
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangBinary value) =
    let length = ByteString.length value in
    if length <= 4294967295 then do
        (liftPut Put.putWord8) tagBinaryExt
        (liftPut Put.putWord32be) $ fromIntegral length
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangBinaryBits (value, 8)) =
    termsToBinary $ OtpErlangBinary value
termsToBinary (OtpErlangBinaryBits (value, bits)) =
    let length = ByteString.length value in
    if length <= 4294967295 then do
        (liftPut Put.putWord8) tagBinaryExt
        (liftPut Put.putWord32be) $ fromIntegral length
        (liftPut Put.putWord8) $ fromIntegral bits
        (liftPut Put.putBuilder) $ Builder.fromByteString value
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangList value) =
    let length = List.length value in
    if length == 0 then do
        (liftPut Put.putWord8) tagNilExt
    else if length <= 4294967295 then do
        (liftPut Put.putWord8) tagListExt
        (liftPut Put.putWord32be) $ fromIntegral length
        termSequenceToBinary value Builder.empty
        (liftPut Put.putWord8) tagNilExt
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangListImproper value) = 
    let length = List.length value in
    if length == 0 then do
        (liftPut Put.putWord8) tagNilExt -- misuse of type
    else if length <= 4294967295 then do
        (liftPut Put.putWord8) tagListExt
        (liftPut Put.putWord32be) $ fromIntegral (length - 1)
        termSequenceToBinary value Builder.empty
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangTuple value) =
    let length = List.length value in
    if length <= 255 then do
        (liftPut Put.putWord8) tagSmallTupleExt
        (liftPut Put.putWord8) $ fromIntegral length
        termSequenceToBinary value Builder.empty
    else if length <= 4294967295 then do
        (liftPut Put.putWord8) tagLargeTupleExt
        (liftPut Put.putWord32be) $ fromIntegral length
        termSequenceToBinary value Builder.empty
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangMap value) =
    let length = Map.size value in
    if length <= 4294967295 then do
        (liftPut Put.putWord8) tagMapExt
        (liftPut Put.putWord32be) $ fromIntegral length
        case Map.foldlWithKey mapPairToBinary (BuilderResult (Right Builder.empty)) value of
            BuilderResult (Left err) ->
                putErrorType err
            BuilderResult (Right builder) ->
                (liftPut Put.putBuilder) builder
    else
        putErrorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangPid (E.Pid nodeTag node eid serial creation)) = do
    (liftPut Put.putWord8) tagPidExt
    (liftPut Put.putWord8) nodeTag
    (liftPut Put.putBuilder) $ Builder.fromByteString node
    (liftPut Put.putBuilder) $ Builder.fromByteString eid
    (liftPut Put.putBuilder) $ Builder.fromByteString serial
    (liftPut Put.putWord8) creation
termsToBinary (OtpErlangPort (E.Port nodeTag node eid creation)) = do
    (liftPut Put.putWord8) tagPortExt
    (liftPut Put.putWord8) nodeTag
    (liftPut Put.putBuilder) $ Builder.fromByteString node
    (liftPut Put.putBuilder) $ Builder.fromByteString eid
    (liftPut Put.putWord8) creation
termsToBinary (OtpErlangReference (E.Reference nodeTag node eid creation)) =
    let length = (ByteString.length eid) `quot` 4 in
    if length == 0 then do
        (liftPut Put.putWord8) tagReferenceExt
        (liftPut Put.putWord8) nodeTag
        (liftPut Put.putBuilder) $ Builder.fromByteString node
        (liftPut Put.putBuilder) $ Builder.fromByteString eid
        (liftPut Put.putWord8) creation
    else if length <= 65535 then do
        (liftPut Put.putWord8) tagNewReferenceExt
        (liftPut Put.putWord16be) $ fromIntegral length
        (liftPut Put.putWord8) nodeTag
        (liftPut Put.putBuilder) $ Builder.fromByteString node
        (liftPut Put.putWord8) creation
        (liftPut Put.putBuilder) $ Builder.fromByteString eid
    else
        putErrorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangFunction (E.Function tag value)) = do
    (liftPut Put.putWord8) tag
    (liftPut Put.putBuilder) $ Builder.fromByteString value

termSequenceToBinary :: [OtpErlangTerm] -> Builder -> PutResult
termSequenceToBinary [] builder = do
    (liftPut Put.putBuilder) builder
termSequenceToBinary (h:t) builder =
    case termsToBinary h of
        PutMResult (Left err) ->
            putErrorType err
        PutMResult (Right put) ->
            let (_, binary) = Put.runPutM put in
            termSequenceToBinary t
                (Builder.append builder (Builder.fromLazyByteString binary))

newtype BuilderResult = BuilderResult { getBuilderResult :: Result Builder }

liftBuilder :: (a -> Builder) -> (a -> BuilderResult)
liftBuilder f = BuilderResult . pure . f

mapPairToBinary :: BuilderResult -> OtpErlangTerm -> OtpErlangTerm ->
    BuilderResult
mapPairToBinary (BuilderResult (Left err)) _ _ =
    BuilderResult $ Left err
mapPairToBinary (BuilderResult (Right builder)) key value =
    case termsToBinary key of
        PutMResult (Left err) ->
            BuilderResult $ Left err
        PutMResult (Right putKey) ->
            case termsToBinary value of
                PutMResult (Left err) ->
                    BuilderResult (Left err)
                PutMResult (Right putValue) ->
                    let (_, binaryKey) = Put.runPutM putKey
                        (_, binaryValue) = Put.runPutM putValue in
                    BuilderResult $ Right $ Builder.append builder
                        (Builder.append
                            (Builder.fromLazyByteString binaryKey)
                            (Builder.fromLazyByteString binaryValue))

