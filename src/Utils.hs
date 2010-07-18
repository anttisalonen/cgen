module Utils
where

import Data.Char

import Text.Regex.Posix
import Safe

toCapital = map toUpper

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

decapitalize []     = []
decapitalize (h:hs) = toLower h : hs

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

getSuffixBy :: Char -> String -> String
getSuffixBy c = fst . foldr go ("", True)
  where go x (acc, True) | x == c    = (acc, False)
                         | otherwise = ((x:acc), True)
        go _ (acc, False)            = (acc, False)

apFst :: (a -> c) -> (a, b) -> (c, b)
apFst fun (a, b) = (fun a, b)

apSnd :: (b -> c) -> (a, b) -> (a, c)
apSnd fun (a, b) = (a, fun b)

for :: [a] -> (a -> b) -> [b]
for = flip map

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

expand :: [(a, [b])] -> [(a, b)]
expand = concat . foldr go []
  where go (a, bs) acc = zip (repeat a) bs : acc

