module ProCon.Generator.ByteString where

import           Control.Monad         (replicateM)
import           Control.Monad.State   (State)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

import           System.Random         (RandomGen)

-- ByteString ベースのジェネレータをいくつか用意するモジュール

-- gen から長さ n の ByteString を生成する (replicateM の代わり)
bytestring :: RandomGen g => Int -> State g Char -> State g ByteString
bytestring n gen = -- 今は思いつかないので replicateM
  BS.pack <$> replicateM n gen
