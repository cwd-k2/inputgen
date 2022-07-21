module ProCon.Generator.Vector where

import           Control.Monad.State (State, get, put)
import           Control.Monad.Trans (lift)
import           System.Random       (RandomGen)

import qualified Data.Set            as S

import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V

import qualified ProCon.Generator    as G

-- Vector ベースのモジュール
-- リストを置き換える目的

-- リストをランダムに並べ替える
-- 思いつかないので fromList
shuffle :: RandomGen g => Vector a -> State g (Vector a)
shuffle v = do
  i <- G.shuffle [0 .. V.length v - 1]
  return $ V.fromList ((v !) <$> i)

-- Vector の中から適当にひとつ選ぶ
element :: RandomGen g => Vector a -> State g a
element a = do
  i <- G.range (0, length a - 1)
  return $ a ! i

-- 全ての要素が互いに異なるリストの生成器を作るコンビネータ
-- 今は思いつかないので fromList
-- 計算量不明
distinct :: (Ord a, RandomGen g)
         => Int
         -> State g a
         -> State g (Vector a)
distinct n gen = V.fromList <$> G.distinct n gen

-- 生成器のリストの中から適当にひとつ選ぶコンビネータ
choice :: RandomGen g => Vector (State g a) -> State g a
choice gens = do
  i <- G.range (0, length gens - 1)
  gens ! i
