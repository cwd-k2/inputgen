{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Main where

import           Control.Monad

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Formatter             (Formattable (..))
import qualified Formatter             as F
import           Generator             (Generator)
import qualified Generator             as G

-- サンプル：レベルアップ問題集『クエリメニュー』経理
data Sample = Sample
  { n   :: Int
  , k   :: Int
  , s   :: [ByteString]
  , apm :: [(ByteString, ByteString, Int)]
  }

instance Formattable Sample where
  format Sample{..} = do
    F.int n >> F.space >> F.int k
    F.newline

    F.mkLines $ F.bytestring <$> s
    F.newline

    F.mkLines $ flip map apm $ \(a, p, m) ->
      F.bytestring a >> F.space >> F.bytestring p >> F.space >> F.int m
    F.newline

sample :: Generator Sample
sample = do
  n <- G.range (1, 100)
  k <- G.range (1, 100)

  s <- G.distinct n $ do
    l <- G.range (1, 20)
    BS.pack <$> replicateM l G.alpha

  q <- G.distinct k $ do
    l <- G.range (1, 11)
    BS.pack <$> replicateM l G.digit

  apm <- forM q $ \p -> do
    (, p, ) <$> G.element s <*> G.range (1, 10_000)

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
