module ProCon.Generator.ByteString where

import           Control.Monad         (replicateM)
import           Control.Monad.State   (State)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

import           System.Random         (RandomGen)

import qualified ProCon.Generator      as G

-- ByteString ベースのジェネレータをいくつか用意するモジュール

-- gen から長さ n の ByteString を生成する (replicateM の代わり)
bytestring :: RandomGen g => Int -> State g Char -> State g ByteString
bytestring n gen = -- 今は思いつかないので replicateM
  BS.pack <$> replicateM n gen

-- ByteString の中から適当にひとつ選ぶ
element :: RandomGen g => ByteString -> State g Char
element s = do
  i <- G.range (0, BS.length s)
  return $ s `BS.index` i

-- ByteString をランダムに並べ替える
-- 思いつかないので fromList
shuffle :: RandomGen g => ByteString -> State g ByteString
shuffle s = do
  i <- G.shuffle [0 .. BS.length s - 1]
  return $ BS.pack ((s `BS.index`) <$> i)

-- 全ての要素が互いに異なる ByteString の生成器を作るコンビネータ
-- 今は思いつかないので fromList
-- 計算量不明
distinct :: RandomGen g
         => Int
         -> State g Char
         -> State g ByteString
distinct n gen = BS.pack <$> G.distinct n gen
