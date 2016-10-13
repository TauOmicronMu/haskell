import Data.Char

import MorseLib

{- Encode a string into Morse, with a shortGap after each letter
   and a mediumGap between each word. -}
encode :: String -> [MorseUnit]
encode [] = []
encode xs = concat [codeWord (toUpper `map` w) ++ mediumGap | w <- (words xs)]

codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (x:xs) = codeSymbol x ++ shortGap ++ codeWord xs

{- Take an array of Beeps/Silences and produce an array of
   dits, dahs, shortGaps and mediumGaps. -}
morseWord :: [MorseUnit] -> [[MorseUnit]] 
morseWord (Beep:Silence:xs)                    = [dit] ++ morseWord xs
morseWord (Beep:Beep:Beep:Silence:xs)          = [dah] ++ morseWord xs 
morseWord (Silence:Silence:Silence:Silence:xs) = [mediumGap] ++ morseWord xs
morseWord (Silence:Silence:xs)                 = [shortGap] ++ morseWord xs
morseWord _                                    = []

{- Concatenate all of the lists that are either side of each
   mediumGap to form the whole morse letters. -}
morseLetters :: [[MorseUnit]] -> [[MorseUnit]]

-- [[B,S],[B,B,B,S],[S,S,S,S],[S,S]] -> [[B,S,B,B,B,S],[S,S,S,S],[S,S]]
