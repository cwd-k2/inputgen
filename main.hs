{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Monad

import           Generator

-- サンプル：レベルアップ問題集『クエリメニュー』経理
data Sample = Sample
  { n   :: Int
  , k   :: Int
  , s   :: [String]
  , apm :: [(String, String, Int)]
  }

sample :: Generator Sample
sample = do
  n <- range (1, 20)
  k <- range (1, 20)

  s <- distinct n $ do
    l <- range (1, 20)
    replicateM l alpha

  q <- distinct k $ do
    l <- range (1, 11)
    replicateM l digit

  apm <- forM q $ \p -> do
    (, p, ) <$> element s <*> range (1, 10_000)

  return Sample{..}

main :: IO ()
main = do
  Sample{..} <- generate sample

  putStrLn $ show n ++ " " ++ show k

  forM_ s putStrLn

  forM_ apm $ \(a, p, m) -> do
    putStrLn $ a ++ " " ++ p ++ " " ++ show m
