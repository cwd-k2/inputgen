module Word8 where

import           Data.Word (Word8)

isUpper, isLower, isDigit, isAlpha, isAlphaNum :: Word8 -> Bool
isUpper w = 0x41 <= w && w <= 0x5a
isLower w = 0x61 <= w && w <= 0x7a
isDigit w = 0x30 <= w && w <= 0x39
isAlpha w = isUpper w || isLower w
isAlphaNum w =
  isAlpha w || isDigit w
