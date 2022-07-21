{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Main where

import           Control.Monad

import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as BS

import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

import           ProCon.Formatter            (Formattable (..))
import qualified ProCon.Formatter            as F

import           ProCon.Generator            (Generator)
import qualified ProCon.Generator            as G
import qualified ProCon.Generator.ByteString as GB
import qualified ProCon.Generator.Vector     as GV

-- サンプル：レベルアップ問題集『クエリメニュー』経理
data Sample = Sample
  { n   :: Int
  , k   :: Int
  , s   :: Vector ByteString
  , apm :: Vector (ByteString, ByteString, Int)
  }

-- 出力形式を定義する
instance Formattable Sample where
  format Sample{..} = do
    F.int n >> F.space >> F.int k
    F.newline

    F.mkLines $ F.bytestring <$> s
    F.newline

    F.mkLines $ flip V.map apm $ \(a, p, m) ->
      F.bytestring a >> F.space >> F.bytestring p >> F.space >> F.int m
    F.newline

-- データ生成方法を指示
sample :: Generator Sample
sample = do
  n <- G.range (1, 100_000)
  k <- G.range (1, 100_000)

  s <- GV.distinct n $ do
    l <- G.range (1, 20)
    GB.bytestring l G.alpha

  q <- GV.distinct k $ do
    l <- G.range (1, 11)
    GB.bytestring l G.digit

  apm <- forM q $ \p -> do
    (, p, ) <$> GV.element s <*> G.range (1, 10_000)

  return Sample{..}

outfiles :: [String]
outfiles =
  [ "1_random_00.in"
  , "1_random_01.in"
  , "1_random_02.in"
  ]

main :: IO ()
main = forM_ outfiles $ \filename -> do
  s <- G.generate sample
  F.writeFile filename s
