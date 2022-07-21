{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Monad
import           Control.Monad.State

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified System.Random         as R

import           Generator

-- サンプル：レベルアップ問題集『クエリメニュー』経理
data Sample = Sample
  { n   :: Int
  , k   :: Int
  , s   :: [BS.ByteString]
  , apm :: [(BS.ByteString, BS.ByteString, Int)]
  }

sample :: (R.RandomGen g) => State g Sample
sample = do
  n <- between (1, 20)
  k <- between (1, 20)

  s <- distinct n $ do
    l <- between (1, 20)
    bytestring l (choice [upper, lower])

  q <- distinct k $ do
    l <- between (1, 11)
    bytestring l digit

  apm <- forM q $ \p -> do
    (, p, ) <$> element s <*> between (1, 10_000)

  return Sample{..}

main :: IO ()
main = do
  g <- R.getStdGen
  -- let g = R.mkStdGen 1024

  let Sample{..} = evalState sample g

  putStrLn $ show n ++ " " ++ show k

  forM_ s BS.putStrLn

  forM_ apm $ \(a, p, m) -> do
    BS.putStr a
    BS.putStr " "
    BS.putStr p
    BS.putStr " "
    print m
