import Data.Char

import ListSplit
import MorseLib

{- Encode a string into Morse, with a shortGap after each letter
   and a mediumGap between each word. -}
encode :: String -> [MorseUnit]
encode [] = []
encode xs = concat [codeWord (toUpper `map` w) ++ mediumGap | w <- (words xs)]

codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (x:xs) = codeSymbol x ++ shortGap ++ codeWord xs


decode :: [MorseUnit] -> String
decode xs = parsePhrase $ morseLetters $ morseWords $ morseUnits xs
 
{- Take an array of Beeps/Silences and produce an array of
   tokens representing dits, dahs, shortGaps and mediumGaps. -}
morseUnits :: [MorseUnit] -> [String] 
morseUnits (Beep:Silence:xs)                    = ["dit"] ++ morseUnits xs
morseUnits (Beep:Beep:Beep:Silence:xs)          = ["dah"] ++ morseUnits xs 
morseUnits (Silence:Silence:Silence:Silence:xs) = ["mediumGap"] ++ morseUnits xs
morseUnits (Silence:Silence:xs)                 = ["shortGap"] ++ morseUnits xs
morseUnits _                                    = []

morseWords :: [String] -> [[String]]
morseWords xs = splitWhen (=="mediumGap") xs

morseLetters :: [[String]] -> [[[String]]]
morseLetters xs = init [splitWhen (=="shortGap") ys | ys <- xs] -- init to remove the trailing silence.

{- Inverse of morseUnits -}
morseList :: [String] -> [MorseUnit]
morseList ("dit":xs)       = dit ++ morseList xs
morseList ("dah":xs)       = dah ++ morseList xs
morseList ("mediumGap":xs) = mediumGap ++ morseList xs
morseList ("shortGap":xs)  = shortGap ++ morseList xs
morseList _                = [] 

parsePhrase ::  [[[String]]] -> String
parsePhrase []     = ""
parsePhrase (x:xs) = parseWord x ++ " " ++ parsePhrase xs
    where 
        parseWord :: [[String]] -> String
        parseWord xs = [l | x <- xs, (c,l) <- table, morseList x == c] 

