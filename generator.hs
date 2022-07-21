{-# LANGUAGE ScopedTypeVariables #-}
module Generator where

import           Control.Monad
import           Control.Monad.State      (State, state)
import           Data.List                (sortOn)
import           Data.Word                (Word8)

import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Internal (w2c)
import qualified Data.Set                 as S

import           System.Random            (Random, RandomGen)
import qualified System.Random            as R

-- 範囲を指定して整数・文字等を生成する
between :: (Random a, RandomGen g) => (a, a) -> State g a
between = state . R.randomR

-- A-Z の 1 文字を生成する
upper :: (RandomGen g) => State g Char
upper = w2c <$> state (R.randomR (0x41, 0x5a))

-- a-z の 1 文字を生成する
lower :: (RandomGen g) => State g Char
lower = w2c <$> state (R.randomR (0x61, 0x7a))

-- 0-9 の 1 文字を生成する
digit :: (RandomGen g) => State g Char
digit = w2c <$> state (R.randomR (0x30, 0x39))

-- リストをランダムに並べ替える
shuffle :: (RandomGen g)
        => [a]
        -> State g [a]
shuffle a = do
  let l = length a
  i :: [Int]
    <- replicateM l (state R.random)
  return $ snd <$> sortOn fst (zip i a)

-- リストの中から適当にひとつ選ぶ
element :: (RandomGen g) => [a] -> State g a
element a = do
  i <- between (0, length a - 1)
  return $ a !! i

-- 条件を満たす乱数生成器を作るコンビネータ
-- 条件を満たすまで生成し続けるので計算量不明
confirm :: (RandomGen g)
        => (a -> Bool)
        -> State g a
        -> State g a
confirm p gen = do
  v <- gen
  -- 条件を満たすまで生成し続ける
  if p v
     then return v
     else confirm p gen

-- 全ての要素が互いに異なるリストの生成器を作るコンビネータ
-- リストを生成しながら要素判定を行う
-- 計算量不明
distinct :: (Ord a, RandomGen g)
         => Int
         -> State g a
         -> State g [a]
distinct n gen = go S.empty n where
  go _ 0 = return []
  go s l = do
    -- 計算量不明
    v <- confirm (`S.notMember` s) gen
    (v :) <$> go (S.insert v s) (l - 1)

-- 生成器のリストの中から適当にひとつ選ぶコンビネータ
choice :: (RandomGen g) => [State g a] -> State g a
choice gens = do
  i <- between (0, length gens - 1)
  gens !! i

-- 長さと文字生成器を指定して文字列を生成するコンビネータ
bytestring :: (RandomGen g)
           => Int
           -> State g Char
           -> State g ByteString
bytestring n gen = BS.pack <$> replicateM n gen
