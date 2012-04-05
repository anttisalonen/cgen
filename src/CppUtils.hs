module CppUtils
where

import Data.List
import Control.Monad.State

import qualified Data.Set as S

import HeaderData
import Utils

isType :: String -> Bool
isType "virtual" = False
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
combChar _  " "         = ""
combChar _  l           = l

-- separate pointer * and ref & from other chars.
-- remove keywords such as virtual, static, etc.
correctType :: String -> String
correctType t =
  let ns = words t
  in case ns of
       []  -> ""
       ms  -> combChar "*&" $ intercalate " " $ filter isType ms

isStatic :: String -> Bool
isStatic n = take 7 n == "static "

isConst :: String -> Bool
isConst n = take 6 n == "const "

stripStatic :: String -> String
stripStatic n | isStatic n = drop 7 n
              | otherwise  = n

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

getEnumValues :: [EnumVal] -> [(String, Int)]
getEnumValues es = evalState go 0
  where go :: State Int [(String, Int)]
        go = mapM f es
        f :: EnumVal -> State Int (String, Int)
        f (EnumVal en ev) = do
          oldval <- get
          let thisval = case ev of
                         Nothing -> oldval
                         Just m  -> case reads m of
                                      [(v, _)] -> v
                                      _        -> oldval
          put $ thisval + 1
          return (en, thisval)

enumReadable :: [EnumVal] -> Bool
enumReadable = all valid . map enumvalue
  where valid Nothing  = True
        valid (Just n) = case (reads :: String -> [(Int, String)]) n of
                           [(_, [])] -> True
                           _        -> False

publicClass :: Object -> Bool
publicClass (ClassDecl _ _ nest _ _) =
  all (== Public) $ map fst nest
publicClass _                        = False

abstractClass :: Object -> Bool
abstractClass (ClassDecl _ _ _ _ objs) =
  any isAbstractFun (map snd objs)
abstractClass _ = False

removeNamespace :: String -> String
removeNamespace = map (\c -> if c == ':' then '_' else c)

stripNamespace :: String -> String
stripNamespace = last . takeWhile (not . null) . iterate (dropWhile (==':') . snd . break (== ':'))

fixNamespace :: [String] -> String -> String
fixNamespace enums n = (if (stripNamespace n) `elem` enums then stripNamespace else removeNamespace) n

