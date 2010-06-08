module Utils
where

import Data.Char

import Text.Regex.Posix
import Safe

toCapital = map toUpper

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

replace :: String -> String -> String -> String
replace old new str = 
  let (s1, s2, s3) = str =~ old
  in if s2 == old
       then s1 ++ new ++ s3
       else str

splitBy :: Char -> String -> [String]
splitBy c str = 
  let (w1, rest) = break (== c) str
  in if null rest
       then if null w1 then [] else [w1]
       else w1 : splitBy c (tailSafe rest)

stripWhitespace :: String -> String
stripWhitespace = snd . foldr go (True, "") . dropWhile (== ' ')
  where go ' ' (True,  acc) = (True, acc)
        go x   (_,     acc) = (False, x:acc)

switch :: (Eq a) => a -> [(a, b)] -> b -> b
switch _ []          df = df
switch v ((n, f):ns) df
  | v == n    = f
  | otherwise = switch v ns df

