-- | Helpers for byte manipulation.
module Lib.Runtime.Byte
  ( Bytes (..),
    overwriteAt,
    readFrom,
  )
where

import qualified Data.Binary.Get as BG
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Word

-- | Types that can be represented as a byte string.
class Bytes a where
  toLittleEndian :: a -> BS.ByteString
  fromLittleEndian :: BS.ByteString -> a

instance Bytes Word8 where
  toLittleEndian = BB.toLazyByteString . BB.word8
  fromLittleEndian = BG.runGet BG.getWord8

instance Bytes Word16 where
  toLittleEndian = BB.toLazyByteString . BB.word16LE
  fromLittleEndian = BG.runGet BG.getWord16le

instance Bytes Word32 where
  toLittleEndian = BB.toLazyByteString . BB.word32LE
  fromLittleEndian = BG.runGet BG.getWord32le

instance Bytes Word64 where
  toLittleEndian = BB.toLazyByteString . BB.word64LE
  fromLittleEndian = BG.runGet BG.getWord64le

instance Bytes Int8 where
  toLittleEndian = BB.toLazyByteString . BB.int8
  fromLittleEndian = BG.runGet BG.getInt8

instance Bytes Int16 where
  toLittleEndian = BB.toLazyByteString . BB.int16LE
  fromLittleEndian = BG.runGet BG.getInt16le

instance Bytes Int32 where
  toLittleEndian = BB.toLazyByteString . BB.int32LE
  fromLittleEndian = BG.runGet BG.getInt32le

instance Bytes Int64 where
  toLittleEndian = BB.toLazyByteString . BB.int64LE
  fromLittleEndian = BG.runGet BG.getInt64le

instance Bytes Float where
  toLittleEndian = BB.toLazyByteString . BB.floatLE
  fromLittleEndian = BG.runGet BG.getFloatle

instance Bytes Double where
  toLittleEndian = BB.toLazyByteString . BB.doubleLE
  fromLittleEndian = BG.runGet BG.getDoublele

-- | Overwrite portion of the bytestring beginning at index.
overwriteAt :: Bytes a => BS.ByteString -> a -> Int64 -> BS.ByteString
overwriteAt s bs idx =
  let overwrite = toLittleEndian bs
      pref = BS.take idx s
      suf = BS.drop (idx + BS.length overwrite) s
   in BS.concat [pref, overwrite, suf]

-- | Read a little endian encoded word/int/float from a bytestring starting at
-- index.
readFrom :: Bytes a => BS.ByteString -> Int64 -> a
readFrom s idx = fromLittleEndian $ BS.drop idx s
