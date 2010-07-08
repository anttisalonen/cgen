{-# LANGUAGE TemplateHaskell #-}
module HaskellGen
where

import Data.List
import Data.Maybe
import Text.Printf
import System.IO
import System.FilePath
import Control.Monad
import qualified Data.Set as S

import Text.Regex.Posix

import HeaderData
import CppUtils
import Utils
import DeriveMod

data Options = Options
  {
    outputdir         :: FilePath
  , interfacefile     :: String
  , excludepatterns   :: [String] 
  , defaultins        :: [String] 
  , defaultouts       :: [String] 
  , inparameters      :: [String] 
  , outparameters     :: [String] 
  }
  deriving (Show)
$(deriveMods ''Options)

-- haskell c type descriptor, e.g. "Ptr CChar"
data HsCType = HsCType {
    hsname  :: String  -- ^ haskell c type, like "CChar"
  , numptrs :: Int     -- ^ number of pointers
  }
  deriving (Show)

-- descriptor on how to convert a haskell type to a c type
data CConv = WithLambda String | CConvFunc String | NoCConv
  deriving (Show)

-- descriptor on how to convert a c type to a haskell type
data HsConv = HsConv String | NoHsConv
  deriving (Show)

data HsFun = HsFun {
    cfilename  :: String            -- ^ c header file
  , cfunname   :: String            -- ^ c function name
  , cparams    :: [HsCType]         -- ^ haskell c type params
  , cretparam  :: HsCType           -- ^ haskell c type for the return type
  , hsfunname  :: String            -- ^ haskell function name
  , hsparams   :: [(CConv, String)]
     -- ^ (how to convert the c type to a haskell type,
     --   which haskell type to convert to)
  , hsrettypes :: [((CConv, String), HsConv)]
     -- ^ ((how to convert the c return types to a haskell type,
     --   which haskell type to convert to),
     -- ^ how to convert the c return types to a haskell type)
  }

haskellGen :: Options -> [(FilePath, [Object])] -> IO ()
haskellGen opts objs = do
    let outdir   = outputdir opts
        excls    = excludepatterns opts
        dins     = defaultins opts ++ ctypes
        douts    = defaultouts opts
        funs     = map (apSnd (filter (\f -> not $ or (map (\e -> funname f =~ e) excls)))) $ map (apSnd getFuns) objs
        alltypes = getAllTypesWithPtr (concatMap snd funs)
        typeValid :: String -> Bool
        typeValid t = not . isJust $ typeValidMsg opts t
        (cpptypes, rejtypes) = S.partition typeValid alltypes
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
            forM_ filefuns $ \fun -> do
                let inparams = map (removeNamespace . stripConst . correctType) $ map vartype (params fun)
                    retparam = removeNamespace . stripConst . correctType $ rettype fun
                case cfunToHsFun opts file fun of
                  Right hsf -> addFun h hsf
                  Left  err -> hPrintf stderr "Function %s discarded:\n\t%s\n" (getObjName fun) err

-- creates the HsFun.
cfunToHsFun :: Options -> FilePath -> Object -> Either String HsFun
cfunToHsFun opts filename (FunDecl fname rt ps _ _ _ _) =
  case catMaybes (map (typeValidMsg opts) (map (correctType . stripConst) (rt:pts))) of
    [] -> Right $
        HsFun filename 
              fname 
              (map cTypeToHsCType pts)
              (cTypeToHsCType rt) 
              (decapitalize $ dropWhile (== '_') $ dropWhile (/= '_') fname)
              (map cTypeToHsType pts)
              ([(cTypeToHsType rt, convRevFunc rt)])
    l  -> Left (intercalate "\n" l)
 where pts = map vartype ps
cfunToHsFun _ _ _ = Left "Given object is not a function"

-- cTypeToHsCType "const char **" = HsCType CChar 2
cTypeToHsCType :: String -> HsCType
cTypeToHsCType "void" = HsCType "()" 0
cTypeToHsCType t      =
  case cTypeToHs t of
    Nothing -> HsCType (capitalize . removeNamespace . correctType . stripExtra . stripConst $ t) (isPtr ct - 1)
    Just t' -> HsCType t' (isPtr ct)
 where ct = correctType . stripConst $ t

-- showHsCType (CChar, 2) = (Ptr (Ptr CChar))
showHsCType :: HsCType -> String
showHsCType h = hsPointerize (numptrs h) (hsname h)

-- cTypeToHsType "const int **" = (CConvFunc "fromIntegral", "Int")
cTypeToHsType :: String -> (CConv, String)
cTypeToHsType "void"  = (NoCConv, "()")
cTypeToHsType t 
  | correctType (stripConst t) == "char*"
     = (WithLambda "withCString", "String")
  | otherwise =
      case join $ fmap cleanCType $ cTypeToHs t of
        Nothing -> (NoCConv, printHsType t)
        Just t' -> (convFunc t, t')

-- creates the Haskell function definition.
hsFunDefinition :: HsFun -> String
hsFunDefinition h = printf "%s %s = %s" 
               (hsfunname h) 
               (intercalate " " (paramList (length (hsparams h)))) 
               (hsFunDef (hsfunname h) (hsparams h) (snd $ head $ hsrettypes h))

  where
    hsFunDef :: String -> [(CConv, String)] -> HsConv -> String
    hsFunDef fn inparams retparam = 
      let ptypes = zip (paramList maxBound) (map (correctType . stripConst . snd) inparams)
          cstrings = filter (\(_, t) -> t == "String") ptypes
          mkCString (pnm, _) = printf "withCString %s $ \\c%s -> \n  " pnm pnm
          resLift = case retparam of
                      NoHsConv -> ""
                      HsConv n -> "liftM " ++ n ++ " $ "
          funcall = cPrefix ++ fn
          funparams = intercalate " " (map paramcall (zip inparams (map fst ptypes)))
          paramcall :: ((CConv, String), String) -> String
          paramcall ((cv, ct), pt) = pprefix ++ pname ++ psuffix
             where pname              = if ct == "String"
                                          then 'c' : pt
                                          else pt
                   (pprefix, psuffix) = case cv of
                                          CConvFunc n  -> ("(" ++ n ++ " ", ")")
                                          _            -> ("", "")
      in concatMap mkCString cstrings ++ " " ++ resLift ++ " " ++ funcall ++ " " ++ funparams

-- prints out the haskell function declaration and definition.
-- TODO: use pouts.
addFun :: Handle -> HsFun -> IO ()
addFun h hsf = do
  -- FFI import
  hPutStrLn h (ffiHsFun hsf)

  -- type signature
  hPutStrLn h (typeSigHsFun hsf)

  -- function definition
  hPutStrLn h (hsFunDefinition hsf)
  hPutStrLn h ""

 where
  ffiHsFun :: HsFun -> String
  ffiHsFun hf = hsFFIFun (cfilename hf) (cfunname hf) (hsfunname hf) (map showHsCType (cparams hf)) (showHsCType (cretparam hf))

  hsFFIFun :: String -> String -> String -> [String] -> String -> String
  hsFFIFun file fn cfn inparams retparam = printf "foreign import ccall \"%s %s\" %s%s :: %sIO %s" 
                            file fn cPrefix cfn
                            (printHsParams inparams)
                            (printHsType retparam)

  typeSigHsFun :: HsFun -> String
  typeSigHsFun hf = 
    printf "%s :: %sIO %s" 
                      (hsfunname hf)
                      (if null (hsparams hf) then "" else intercalate " -> " (map snd $ hsparams hf) ++ " -> ")
                      (pref ++ intercalate ", " hsr ++ suff)
   where (pref, suff) = if length hsr == 1 then ("", "") else ("(", ")")
         hsr          = map (snd . fst) $ hsrettypes hf

-- checks whether the given type can be used.
-- The type can be used, if:
-- The type is a standard c type either as a value or
-- as a pointer with a defined meaning, or
-- the type is a pointer to a handle, or
-- the type is void.
--
-- The meaning (in/out/in+out) of a standard c type is "defined", if
-- the type is not a pointer, or
-- the type is a pointer, and the meaning is given. (not yet implemented)
typeValidMsg :: Options -> String -> Maybe String
typeValidMsg opts t = validateAll [(t /= "void*", "Void pointer not supported"),
                    (not (isTemplate t), "Template types not supported"),
                    ((t == "void" || isStdType t || isPtr t > 0), "type " ++ t ++ " is a value of a non-standard type"),
                    (hasDir, "direction for the type " ++ t ++ " has not been defined in the interface file")]
  where validateAll = foldl' validate Nothing
          where validate (Just n) _        = Just n
                validate Nothing  (f, str) = if not f then Just str else Nothing
        hasDir = isPtr t == 0 ||
                   (isPtr t == 1 && (not $ isJust $ cTypeToHs t)) ||
                   t `elem` defaultins opts

importForeign :: String
importForeign = "import Foreign\nimport Foreign.C.String\nimport Foreign.C.Types\n"

cPrefix :: String
cPrefix = "c_"

-- in: a c type, like "int"
-- out: the haskell conversion function for converting from haskell data type
convFunc :: String -> CConv
convFunc ptype =
  case fromMaybe "" $ cTypeToHs ptype of
    "CChar"   -> CConvFunc "castCCharToChar" 
    "CSChar"  -> CConvFunc "fromIntegral" 
    "CUChar"  -> CConvFunc "fromIntegral" 
    "CShort"  -> CConvFunc "fromIntegral" 
    "CUShort" -> CConvFunc "fromIntegral" 
    "CInt"    -> CConvFunc "fromIntegral" 
    "CUInt"   -> CConvFunc "fromIntegral" 
    "CSize"   -> CConvFunc "fromIntegral" 
    "CLong"   -> CConvFunc "fromIntegral" 
    "CULong"  -> CConvFunc "fromIntegral" 
    "CFloat"  -> CConvFunc "realToFrac" 
    "CDouble" -> CConvFunc "realToFrac" 
    "CBool"   -> CConvFunc "fromBool" 
    _         -> NoCConv

-- in: a c type, like "int"
-- out: the haskell conversion function for converting to haskell data type
convRevFunc :: String -> HsConv
convRevFunc t
  | fromMaybe "" (cTypeToHs t) == "CBool" = HsConv "toBool"
  | otherwise = 
     let ccf = convFunc t
     in case ccf of
          CConvFunc n -> HsConv n
          _           -> NoHsConv

paramList :: Int -> [String]
paramList n = map ('p':) (map show [1..n])

printParamList :: [String] -> String
printParamList = intercalate " "

-- printHsType "const char **" = "(Ptr (Ptr CChar))"
printHsType :: String -> String
printHsType "void" = "()"
printHsType t      = 
  case cTypeToHs t of
    Nothing -> hsPointerize (isPtr ct - 1) $ capitalize . removeNamespace . correctType . stripConst $ t
    Just t' -> hsPointerize (isPtr ct) t'
 where ct = correctType . stripConst $ t

hsPointerize :: Int -> String -> String
hsPointerize numPtrs t = 
  concat (replicate numPtrs "(Ptr ") ++ (stripPtr t) ++ concat (replicate numPtrs ")")

cTypeToHs :: String -> Maybe String
cTypeToHs = cTypeToHs' . clearType
    where -- strips const, pointers: const char ** => char
      clearType :: String -> String
      clearType = stripPtr . correctType . stripConst

cTypeToHs' :: String -> Maybe String
cTypeToHs' "float" = Just "CFloat"
cTypeToHs' "double" = Just "CDouble"
cTypeToHs' "char" = Just "CChar"
cTypeToHs' "int" = Just "CInt"
cTypeToHs' "unsigned int" = Just "CUInt"
cTypeToHs' "signed int" = Just "CInt"
cTypeToHs' "long" = Just "CLong"
cTypeToHs' "unsigned long" = Just "CULong"
cTypeToHs' "signed long" = Just "CLong"
cTypeToHs' "bool" = Just "CBool"
cTypeToHs' "short" = Just "CShort"
cTypeToHs' "unsigned short" = Just "CUShort"
cTypeToHs' "signed short" = Just "CShort"
cTypeToHs' "unsigned" = Just "CInt"
cTypeToHs' "long long" = Just "CLong"
cTypeToHs' "unsigned long long" = Just "CULong"
cTypeToHs' "int8_t" = Just "CChar"
cTypeToHs' "uint8_t" = Just "CUChar"
cTypeToHs' "int16_t" = Just "CShort"
cTypeToHs' "uint16_t" = Just "CUShort"
cTypeToHs' "int32_t" = Just "CLong"
cTypeToHs' "uint32_t" = Just "CULong"
cTypeToHs' "int64_t" = Just "CLong"
cTypeToHs' "uint64_t" = Just "CULong"
cTypeToHs' "size_t" = Just "CSize"
cTypeToHs' "uint8" = Just "CUChar"
cTypeToHs' "uint16" = Just "CUShort"
cTypeToHs' "uint32" = Just "CULong"
cTypeToHs' "uint64" = Just "CULong"
cTypeToHs' _ = Nothing

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

printHsParams :: [String] -> String
printHsParams [] = ""
printHsParams types = 
  intercalate " -> " (map printHsType types) ++ " -> " 

removeNamespace :: String -> String
removeNamespace = map (\c -> if c == ':' then '_' else c)

