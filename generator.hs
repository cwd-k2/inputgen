{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified System.Random       as R

-- 条件を満たす乱数生成器を作るコンビネータ
-- 条件を満たすまで生成し続けるので計算量不明
confirm :: (R.RandomGen g)
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
distinct :: (Eq a, R.RandomGen g)
         => Int
         -> State g a
         -> State g [a]
distinct n gen = go [] n where
  go a 0 = return a
  go a l = do
    -- こいつのせいで計算量がわからない
    v <- confirm (`notElem` a) gen
    go (v : a) (l - 1)

-- リストをランダムに並べ替える
shuffle :: (R.RandomGen g)
        => [a]
        -> State g [a]
shuffle a = do
  -- こいつのせいで計算量がわからない
  -- distinct を使わない方法があるかもしれない
  i <- distinct (length a) (state $ R.randomR (0, length a - 1))
  return $ snd <$> sortOn fst (zip i a)

-- リストの中から適当にひとつ選ぶ
element :: (R.RandomGen g)
        => [a]
        -> State g a
element a = do
  i <- state $ R.randomR (0, length a - 1)
  return $ a !! i

-- 生成器のリストの中から適当にひとつ選ぶコンビネータ
choice :: (R.RandomGen g)
       => [State g a]
       -> State g a
choice gens = do
  i <- state $ R.randomR (0, length gens - 1)
  gens !! i

main :: IO ()
main = do
  print 1
