module HaskellGen (haskellGen)
where

import Data.List
import Data.Maybe
import Text.Printf
import System.IO
import System.FilePath
import Control.Monad
import qualified Data.Set as S

import HeaderData
import CppUtils
import Utils

apSnd :: (b -> c) -> (a, b) -> (a, c)
apSnd fun (a, b) = (a, fun b)

haskellGen :: FilePath -> [(FilePath, [Object])] -> IO ()
haskellGen outdir objs = do
    let funs     = map (apSnd getFuns) objs
        alltypes = getAllTypesWithPtr (concatMap snd funs)
        (cpptypes, rejtypes) = S.partition (\t -> t /= "void" && 
                                           t /= "void*" && 
                                           not (isTemplate t) && 
                                           (isStdType t || isPtr t > 0))
                                          alltypes
        ctypes = map removeNamespace (S.toList cpptypes)
    hPutStrLn stderr $ "Rejected types: "
    forM_ (S.toList rejtypes) print
    hPutStrLn stderr $ "Used types: "
    forM_ ctypes print
    let hstypes = nub . map capitalize $ filter (not . isStdType) $ map stripPtr ctypes
        typefile = outdir </> "Types.hs"
    withFile typefile WriteMode $ \h -> do
        hPrintf h "module Types\nwhere\n\n"
        hPutStrLn h importForeign
        hPrintf h "type CBool = CChar -- correct?\n\n"
        forM_ hstypes $ \t -> do
            hPrintf h "newtype %s = %s (Ptr %s) -- nullary data type\n" t t t

    forM_ funs $ \(file, filefuns) ->
        withFile (outdir </> ((takeBaseName file) ++ ".hs")) WriteMode $ \h -> do
            hPrintf h "{-# LANGUAGE ForeignFunctionInterface #-}\n"
            hPrintf h "module %s\nwhere\n\nimport Types\nimport Control.Monad\n\n" (takeBaseName file)
            hPutStrLn h importForeign
            forM_ filefuns $ \fun -> 
                if (all (\t -> isStdType (clearType t) || hsType t `elem` hstypes || t == "void") (getUsedFunTypes fun)) 
                  then
                    addFun h file fun
                  else
                    hPrintf stderr "Function %s discarded:\n\t%s\n" (getObjName fun) (hsFFIFun file fun)

importForeign :: String
importForeign = "import Foreign\nimport Foreign.C.String\nimport Foreign.C.Types\n"

cPrefix :: String
cPrefix = "c_"

addFun :: Handle -> String -> Object -> IO ()
addFun h file fun = do
  -- FFI import
  hPrintf h (hsFFIFun file fun)

  -- type signature
  hPrintf h "%s :: %sIO %s\n" 
                    hsfunname
                    (printExportedHsParams (params fun)) 
                    (printExportedHsType (rettype fun))

  -- function definition
  hPrintf h "%s %s = %s\n" 
      hsfunname
      (printParamList plist)
      (hsFunDef fun)
  hPrintf h "\n"
 where plist :: [String]
       plist = paramList (length (params fun))
       hsfunname = decapitalize $ dropWhile (== '_') $ dropWhile (/= '_') (funname fun)

hsFunDef :: Object -> String
hsFunDef (FunDecl fn rt ps _ _ _ _) = 
  let ptypes = zip (paramList maxBound) $ map (\(ParamDecl _ pt _ _) -> correctType . stripConst $ pt) ps
      cstrings = filter (\(_, t) -> t == "char*") ptypes
      mkCString (pnm, _) = printf "withCString %s $ \\c%s -> \n  " pnm pnm
      resLift = if null (convRevFunc rt) then "" else "liftM " ++ convRevFunc rt ++ " $ "
      funcall = cPrefix ++ fn
      funparams = intercalate " " (map paramcall ptypes)
      paramcall :: (String, String) -> String
      paramcall (pn, pt) = pprefix ++ pname ++ psuffix
         where pname              = if pt == "char*" then ('c':pn) else pn
               (pprefix, psuffix) = case convFunc pt of
                                      "" -> ("", "")
                                      s  -> ("(" ++ s ++ " ", ")")
  in concatMap mkCString cstrings ++ " " ++ resLift ++ " " ++ funcall ++ " " ++ funparams
hsFunDef _                         = "undefined"

convFunc :: String -> String
convFunc ptype =
  case fromMaybe "" $ cTypeToHs ptype of
    "CChar"   -> "castCCharToChar" 
    "CSChar"  -> "fromIntegral" 
    "CUChar"  -> "fromIntegral" 
    "CShort"  -> "fromIntegral" 
    "CUShort" -> "fromIntegral" 
    "CInt"    -> "fromIntegral" 
    "CUInt"   -> "fromIntegral" 
    "CSize"   -> "fromIntegral" 
    "CLong"   -> "fromIntegral" 
    "CULong"  -> "fromIntegral" 
    "CFloat"  -> "realToFrac" 
    "CDouble" -> "realToFrac" 
    "CBool"   -> "fromBool" 
    _         -> ""

convRevFunc :: String -> String
convRevFunc t
  | fromMaybe "" (cTypeToHs t) == "CBool" = "toBool"
  | otherwise = convFunc t

paramList :: Int -> [String]
paramList n = map ('p':) (map show [1..n])

printParamList :: [String] -> String
printParamList = intercalate " "

printExportedHsType :: String -> String
printExportedHsType "void"  = "()"
printExportedHsType t 
  | correctType (stripConst t) == "char*"
     = "String"
  | otherwise =
      case join $ fmap cleanCType $ cTypeToHs (clearType t) of
        Nothing -> printHsType t
        Just t' -> t'

clearType :: String -> String
clearType = stripPtr . correctType . stripConst

hsFFIFun :: String -> Object -> String
hsFFIFun file fun = printf "foreign import ccall \"%s %s\" %s%s :: %sIO %s\n" 
                          file (getObjName fun) cPrefix (getObjName fun)
                          (printHsParams (params fun))
                          (printHsType (rettype fun))

hsType :: String -> String
hsType "void" = "()"
hsType t      = capitalize . stripPtr . removeNamespace . correctType . stripConst $ t

printHsType :: String -> String
printHsType "void" = "()"
printHsType t      = 
  case cTypeToHs (clearType t) of
    Nothing -> hsPointerize (isPtr ct - 1) $ capitalize . removeNamespace . correctType . stripConst $ t
    Just t' -> hsPointerize (isPtr ct) t'
 where ct = correctType . stripConst $ t

hsPointerize :: Int -> String -> String
hsPointerize numptrs t = 
  concat (replicate numptrs "(Ptr ") ++ (stripPtr t) ++ concat (replicate numptrs ")")

cTypeToHs :: String -> Maybe String
cTypeToHs "float" = Just "CFloat"
cTypeToHs "double" = Just "CDouble"
cTypeToHs "char" = Just "CChar"
cTypeToHs "int" = Just "CInt"
cTypeToHs "unsigned int" = Just "CUInt"
cTypeToHs "signed int" = Just "CInt"
cTypeToHs "long" = Just "CLong"
cTypeToHs "unsigned long" = Just "CULong"
cTypeToHs "signed long" = Just "CLong"
cTypeToHs "bool" = Just "CBool"
cTypeToHs "short" = Just "CShort"
cTypeToHs "unsigned short" = Just "CUShort"
cTypeToHs "signed short" = Just "CShort"
cTypeToHs "unsigned" = Just "CInt"
cTypeToHs "long long" = Just "gLong"
cTypeToHs "unsigned long long" = Just "CULong"
cTypeToHs "int8_t" = Just "CChar"
cTypeToHs "uint8_t" = Just "CUChar"
cTypeToHs "int16_t" = Just "CShort"
cTypeToHs "uint16_t" = Just "CUShort"
cTypeToHs "int32_t" = Just "CLong"
cTypeToHs "uint32_t" = Just "CULong"
cTypeToHs "int64_t" = Just "CLong"
cTypeToHs "uint64_t" = Just "CULong"
cTypeToHs "size_t" = Just "CSize"
cTypeToHs "uint8" = Just "CUChar"
cTypeToHs "uint16" = Just "CUShort"
cTypeToHs "uint32" = Just "CULong"
cTypeToHs "uint64" = Just "CULong"
cTypeToHs _ = Nothing

cleanCType :: String -> Maybe String
cleanCType "CFloat"      = Just "Float"
cleanCType "CDouble"     = Just "Double"
cleanCType "CChar"       = Just "Char"
cleanCType "CSChar"      = Just "Int"
cleanCType "CUChar"      = Just "Int"
cleanCType "CInt"        = Just "Int"
cleanCType "CUInt"       = Just "Int"
cleanCType "CLong"       = Just "Int"
cleanCType "CULong"      = Just "Int"
cleanCType "CBool"       = Just "Bool"
cleanCType "CShort"      = Just "Int"
cleanCType "CUShort"     = Just "Int"
cleanCType "CSize"       = Just "Int"
cleanCType _ = Nothing

printExportedHsParams :: [ParamDecl] -> String
printExportedHsParams [] = ""
printExportedHsParams ps = 
  let types = map vartype ps
  in intercalate " -> " (map printExportedHsType types) ++ " -> " 

printHsParams :: [ParamDecl] -> String
printHsParams [] = ""
printHsParams ps = 
  let types = map vartype ps
  in intercalate " -> " (map printHsType types) ++ " -> " 

removeNamespace :: String -> String
removeNamespace = map (\c -> if c == ':' then '_' else c)

