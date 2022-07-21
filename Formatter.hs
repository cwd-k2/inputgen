{-# LANGUAGE FlexibleInstances #-}
module Formatter
  ( Formatter
  , Formattable (..)
  , int
  , double
  , char
  , string
  , bytestring
  , space
  , newline
  , joinWith
  , mkWords
  , mkLines
  , writeFile
  ) where

import           Prelude                 hiding (writeFile)

import           Control.Monad.State     (State, execState, state)
import           System.IO               (Handle, IOMode (..), hClose, openFile)

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as BB

-- 出力形式を整えるためのモナドによる DSL を提供する
type Formatter = State BB.Builder ()

-- writeFile やコンビネータを利用するためのクラス
-- 出力形式を整える
class Formattable a where
  format :: a -> Formatter

mkFormat :: (a -> BB.Builder) -> a -> Formatter
mkFormat f a = state $ \s -> ((), s <> f a)

-- ファイルに出力するまでが仕事
writeFile :: Formattable a => FilePath -> a -> IO ()
writeFile path a = do
  h <- openFile path WriteMode
  let b = execState (format a) mempty
  BB.hPutBuilder h b
  hClose h

-- 基本的なフォーマッタ
int :: Int -> Formatter
int = mkFormat BB.intDec

double :: Double -> Formatter
double = mkFormat BB.doubleDec

char :: Char -> Formatter
char = mkFormat BB.charUtf8

string :: String -> Formatter
string = mkFormat BB.stringUtf8

bytestring :: ByteString -> Formatter
bytestring = mkFormat BB.byteString

instance Formattable Int where
  format = int

instance Formattable Double where
  format = double

instance Formattable Char where
  format = char

instance Formattable String where
  format = string

instance Formattable ByteString where
  format = bytestring

-- ヘルパ
space :: Formatter
space = char ' '

newline :: Formatter
newline = char '\n'

-- 以下コンビネータ
joinWith :: Formatter -> [Formatter] -> Formatter
joinWith f = foldl1 (\l r -> l >> f >> r)

mkWords :: [Formatter] -> Formatter
mkWords = joinWith space

mkLines :: [Formatter] -> Formatter
mkLines = joinWith newline
