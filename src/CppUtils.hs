module CppUtils
where

import qualified Data.Set as S

import HeaderData
import Utils

isConst :: String -> Bool
isConst n = take 6 n == "const "

-- stripPtr " char * " = "char"
stripPtr :: String -> String
stripPtr = stripWhitespace . takeWhile (/= '*')

stripConst :: String -> String
stripConst n | isConst n = stripWhitespace $ drop 5 n 
             | otherwise = n

getAllTypes :: [Object] -> S.Set String
getAllTypes = S.fromList . map (stripConst . stripPtr) . concatMap getUsedFunTypes

