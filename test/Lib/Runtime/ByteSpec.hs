module Lib.Runtime.ByteSpec where

import qualified Data.ByteString.Lazy as BS
import Data.Word
import Lib.Runtime.Byte
import Test.Hspec

spec :: Spec
spec = do
  describe "overwriteAt" $ do
    let bs = BS.pack [0, 1, 2, 3]

    it "should overwrite beginning" $ do
      let v = 8 :: Word8
      let out = overwriteAt bs v 0
      out `shouldBe` BS.pack [8, 1, 2, 3]

    it "should overwrite end" $ do
      let v = 8 :: Word8
      let out = overwriteAt bs v 3
      out `shouldBe` BS.pack [0, 1, 2, 8]

    it "should overwrite multiple" $ do
      let v = 65535 :: Word16
      let out = overwriteAt bs v 1
      out `shouldBe` BS.pack [0, 255, 255, 3]

    it "should overwrite using little endian" $ do
      let v = 3 :: Word16
      let out = overwriteAt bs v 0
      out `shouldBe` BS.pack [3, 0, 2, 3]

  describe "readFrom" $ do
    it "should read multi-byte word" $ do
      let bs = BS.pack [3, 255, 255, 8]
      let out = readFrom bs 1 :: Word16
      out `shouldBe` 65535
