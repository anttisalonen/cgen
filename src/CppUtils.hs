module CppUtils
where

import Data.List

import qualified Data.Set as S

import HeaderData
import Utils

isType :: String -> Bool
isType "virtual" = False
isType "static"  = False
isType "enum"    = False
isType "mutable" = False
isType "struct"  = False
isType "union"   = False
isType "inline"  = False
isType _         = True

combChars :: String -> [String] -> [String]
combChars st = map (combChar st)

combChar :: String -> String -> String
combChar st (x:y:xs)
  | x == ' ' && y == ' '    = combChar st xs
  | x == ' ' && y `elem` st = y : combChar st xs
  | otherwise               = x : combChar st (y:xs)
combChar _  l           = l

-- separate pointer * and ref & from other chars.
-- remove keywords such as virtual, static, etc.
correctType :: String -> String
correctType t =
  let ns = words t
  in case ns of
       []  -> ""
       ms  -> intercalate " " $ combChars "&" $ combChars "*" $ filter isType ms

isConst :: String -> Bool
isConst n = take 6 n == "const "

stripExtra :: String -> String
stripExtra = stripConst . stripRef . stripPtr

stripChar :: Char -> String -> String
stripChar c = stripWhitespace . takeWhile (/= c)

-- stripPtr " char * " = "char"
stripPtr :: String -> String
stripPtr = stripChar '*'

stripRef :: String -> String
stripRef = stripChar '&'

stripConst :: String -> String
stripConst n | isConst n = stripWhitespace $ drop 5 n 
             | otherwise = n

getAllTypes :: [Object] -> S.Set String
getAllTypes = S.fromList . map (stripConst . stripPtr) . concatMap getUsedFunTypes

getAllTypesWithPtr :: [Object] -> S.Set String
getAllTypesWithPtr = S.fromList . map (correctType . stripConst) . concatMap getUsedFunTypes

-- "aaa < bbb, ddd> fff" = " bbb, ddd"
betweenAngBrackets :: String -> String
betweenAngBrackets = fst . foldr go ("", Nothing)
  where go _   (accs, Just True)  = (accs, Just True)    -- done
        go '>' (accs, Nothing)    = (accs, Just False)   -- start
        go '<' (accs, Just False) = (accs, Just True)    -- finish
        go c   (accs, Just False) = (c:accs, Just False) -- collect
        go _   (accs, Nothing)    = (accs, Nothing)      -- continue

isTemplate :: String -> Bool
isTemplate = not . null . betweenAngBrackets

isPtr :: String -> Int
isPtr = length . filter (=='*') . dropWhile (/= '*')

isStdType "float" = True
isStdType "double" = True
isStdType "char" = True
isStdType "int" = True
isStdType "unsigned int" = True
isStdType "signed int" = True
isStdType "long" = True
isStdType "unsigned long" = True
isStdType "signed long" = True
isStdType "bool" = True
isStdType "short" = True
isStdType "unsigned short" = True
isStdType "signed short" = True
isStdType "unsigned" = True
isStdType "long long" = True
isStdType "unsigned long long" = True
isStdType "int8_t" = True
isStdType "uint8_t" = True
isStdType "int16_t" = True
isStdType "uint16_t" = True
isStdType "int32_t" = True
isStdType "uint32_t" = True
isStdType "int64_t" = True
isStdType "uint64_t" = True
isStdType "size_t" = True
isStdType "uint8" = True
isStdType "uint16" = True
isStdType "uint32" = True
isStdType "uint64" = True
isStdType _ = False

