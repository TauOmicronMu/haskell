import Data.Char

import MorseLib

{- Encode a string into Morse, with a shortGap after each letter
   and a mediumGap between each word. -}
encode :: String -> [MorseUnit]
encode [] = []
encode xs = concat [codeWord (toUpper `map` w) ++ shortGap | w <- (words xs)]

codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (x:xs) = codeSymbol x ++ mediumGap ++ codeWord xs 
