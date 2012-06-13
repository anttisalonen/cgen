{-# LANGUAGE TemplateHaskell #-}
module HaskellGen
where

import Data.List
import Data.Maybe
import Text.Printf
import System.IO
import System.FilePath
import Control.Monad
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M

import Text.Regex.Posix
import Safe

import HeaderData
import CppUtils
import Utils
import DeriveMod

data Options = Options
  {
    outputdir         :: FilePath
  , interfacefile     :: String
  , inheritfile       :: FilePath
  , umbrellamodule    :: FilePath
  , excludepatterns   :: [String] 
  , defaultins        :: [String] 
  , defaultouts       :: [String] 
  , inparameters      :: [String] 
  , outparameters     :: [String] 
  , hierarchy         :: String
  }
  deriving (Show)
$(deriveMods ''Options)

defaultOptions :: Options
defaultOptions = Options "" "" "" "" [] [] [] [] [] ""

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
  deriving (Show)

haskellGen :: Options -> [(FilePath, [Object])] -> IO ()
haskellGen opts objs = do
    let outdir   = outputdir opts
        excls    = excludepatterns opts
        funs     = map (apSnd (filter (\f -> not $ or (map (\e -> funname f =~ e) excls)))) $ map (apSnd getFuns) objs
        enums    = concatMap getEnums $ map snd objs
        enumnames = map (capitalize . enumname) enums
        alltypes = getAllTypesWithPtr (concatMap snd funs)
        typeValid :: String -> Bool
        typeValid t = not . isJust $ typeValidMsg enumnames opts t
        (cpptypes, rejtypes) = S.partition typeValid alltypes
        modprefix = hierarchy opts
    hPutStrLn stderr $ "Rejected types: "
    forM_ (S.toList rejtypes) print
    hPutStrLn stderr $ "Used types: "
    let hstypes = nubBy (\x y -> hstypify x == hstypify y) $ filter (not . isStdType . stripPtr) (S.toList cpptypes)
        typefile = outdir </> "Types.hs"
        hstypify = capitalize . stripPtr . removeNamespace

    -- Types module
    withFile typefile WriteMode $ \h -> do
        hPrintf h "module %sTypes\nwhere\n\n" modprefix
        hPutStrLn h importForeign
        hPrintf h "type CBool = CChar -- correct?\n\n"
        forM_ hstypes $ \t -> do
            case (filter (\e -> enumname e == (capitalize . stripPtr . stripNamespace) t)) enums of
              ((EnumDef en vs _):_) -> do
                let hstypename = hstypify en
                    decaptn    = decapitalize hstypename
                    constrs    = map (apFst hstypify) $ getEnumValues vs
                hPrintf h "\ndata %s = %s\n\n" hstypename (intercalate " | " (map fst constrs))
                hPrintf h "%sToCInt :: %s -> CInt\n" decaptn hstypename
                forM_ constrs $ \(c, v) -> do
                    hPrintf h "%sToCInt %s = %d\n" decaptn c v
                hPrintf h "\n"
                hPrintf h "cintTo%s :: CInt -> %s\n" hstypename hstypename
                forM_ constrs $ \(c, v) -> do
                    hPrintf h "cintTo%s %d = %s\n" hstypename v c
                hPrintf h "cintTo%s n = error $ \"cintTo%s: can not convert integer '\" ++ show n ++ \"' to %s\"\n" hstypename hstypename hstypename
                hPrintf h "\n"
              _                     -> let t' = hstypify t in hPrintf h "newtype %s = %s (Ptr %s) -- nullary data type\n" t' t' t'
        hPrintf h "\n"

        -- classes and instances
        when (not . null $ inheritfile opts) $ do
            inheritdata <- withFile (inheritfile opts) ReadMode $ \ih -> do
                conts <- hGetContents ih
                forM (lines conts) $ \l -> do
                    let (cname, inheritline) = break (== '|') l
                        inherits = map (dropWhile (== ',')) $ groupBy (\_ b -> b /= ',') $ tailSafe inheritline
                    return (cname, inherits)

            let hstypeset = (S.\\) (S.fromList (map hstypify hstypes)) (S.fromList enumnames) 
                inheritlist :: [(String, [String])]
                inheritlist = M.toList . foldr (\(k, a) acc -> M.insertWith (++) k [a] acc) M.empty . map swap . expand . catMaybes $ 
                  for inheritdata $ \(cname, superclasses) ->
                      if hstypify cname `S.member` hstypeset 
                         then Just (hstypify cname, catMaybes $ for superclasses $ \s ->
                                     if (s `S.member` hstypeset) then Just (hstypify s) else Nothing)
                         else Nothing
            forM_ inheritlist $ \(cname, inheritances) -> do
                when (cname `S.member` hstypeset && not (null inheritances)) $ do
                    hPrintf h "class C%s a where\n  to%s :: a -> %s\n\n" cname cname cname
                    forM_ inheritances $ \i -> do
                         hPrintf h "instance C%s %s where\n  to%s (%s p) = %s (castPtr p)\n\n" cname i cname i cname

    expfuns <- forM funs $ \(file, filefuns) ->
        withFile (outdir </> ((takeBaseName file) ++ ".hs")) WriteMode $ \h -> do
            allgenfuns <- catMaybes <$> (forM filefuns $ \fun -> do
                case cfunToHsFun enumnames opts file fun of
                  Right hsf -> return $ Just hsf
                  Left  err -> hPrintf stderr "Function %s discarded:\n\t%s\n" (getObjName fun) err >> return Nothing)
            let constructors = filter isConstructor allgenfuns
                withfunnames = map withFunName constructors
            hPrintf h "{-# LANGUAGE ForeignFunctionInterface #-}\n"
            hPrintf h "module %s%s(\n%s\n)\n\nwhere\n\nimport %sTypes\nimport Control.Monad\n\n" 
                      modprefix 
                      (takeBaseName file) 
                      (intercalate ", \n" $ withfunnames ++ map hsfunname allgenfuns)
                      modprefix
            hPutStrLn h importForeign
            mapM_ (addWithFun h) constructors
            forM_ allgenfuns $ addFun h
            return $ withfunnames ++ map hsfunname allgenfuns

    when (not . null $ umbrellamodule opts) $ do
        withFile (outdir </> (umbrellamodule opts)) WriteMode $ \h -> do
            hPrintf h "module %s%s(\n  %s\n)\n\nwhere\n\n%s\n" 
                      modprefix
                      (takeBaseName $ umbrellamodule opts)
                      (intercalate ", \n  " $ concat expfuns)
                      (intercalate "\n" $ map (("import " ++ modprefix) ++) (map (takeBaseName . fst) funs))

withFunName :: HsFun -> String
withFunName = replace "new" "with" . hsfunname

-- creates the HsFun.
cfunToHsFun :: [String] -> Options -> FilePath -> Object -> Either String HsFun
cfunToHsFun enumnames opts filename (FunDecl fname rt ps _ _ _ _) =
  case catMaybes (map (typeValidMsg enumnames opts) (map (correctType . stripConst) (rt:pts))) of
    [] -> Right $
        HsFun filename 
              fname 
              (map (cTypeToHsCType enumnames) pts)
              (cTypeToHsCType enumnames rt) 
              (decapitalize $ if '_' `elem` fname then dropWhile (== '_') $ dropWhile (/= '_') fname else fname)
              (map (cTypeToHsType enumnames) pts)
              ([(cTypeToHsType enumnames rt, convRevFunc enumnames rt)])
    l  -> Left (intercalate "\n" l)
 where pts = map vartype ps
cfunToHsFun _ _ _ _ = Left "Given object is not a function"

-- cTypeToHsCType "const char **" = HsCType CChar 2
cTypeToHsCType :: [String] -> String -> HsCType
cTypeToHsCType _     "void" = HsCType "()" 0
cTypeToHsCType enums t      
  | (stripNamespace . correctType . stripExtra . stripConst $ t) `elem` enums = HsCType "CInt" 0
  | otherwise =
      case cTypeToHs t of
        Nothing -> HsCType (capitalize . removeNamespace . correctType . stripExtra . stripConst $ t) (isPtr ct - 1)
        Just t' -> HsCType t' (isPtr ct)
     where ct = correctType . stripConst $ t

-- showHsCType (CChar, 2) = (Ptr (Ptr CChar))
showHsCType :: HsCType -> String
showHsCType h = hsPointerize (numptrs h) (hsname h)

-- cTypeToHsType "const int **" = (CConvFunc "fromIntegral", "Int")
cTypeToHsType :: [String] -> String -> (CConv, String)
cTypeToHsType _     "void"  = (NoCConv, "()")
cTypeToHsType enums t 
  | correctType (stripConst t) == "char*"
     = (WithLambda "withCString", "String")
  | otherwise =
      let entype = stripNamespace . correctType . stripExtra . stripConst $ t
      in if entype `elem` enums
           then (CConvFunc (printf "%sToCInt" $ decapitalize entype), entype)
           else
             case join $ fmap cleanCType $ cTypeToHs t of
               Nothing -> (NoCConv, printHsType enums t)
               Just t' -> (convFunc t, t')

paramList :: HsFun -> String
paramList = intercalate " " . paramNames . length . hsparams

-- creates the Haskell function definition.
hsFunDefinition :: HsFun -> String
hsFunDefinition h = printf "%s %s = %s" 
               (hsfunname h) 
               (paramList h)
               (hsFunDef (hsfunname h) (hsparams h) (snd $ head $ hsrettypes h))

  where
    hsFunDef :: String -> [(CConv, String)] -> HsConv -> String
    hsFunDef fn inparams retparam = 
      let ptypes = zip (paramNames maxBound) (map (correctType . stripConst . snd) inparams)
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
-- TODO: take defined out-params into account.
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
                            (retparam)

  typeSigHsFun :: HsFun -> String
  typeSigHsFun hf = 
    printf "%s :: %sIO %s" 
                      (hsfunname hf)
                      (typeSigList hf)
                      (hsFunRetType hf)

hsFunRetType :: HsFun -> String
hsFunRetType hf =
  let hsr          = map (snd . fst) $ hsrettypes hf
      (pref, suff) = if length hsr == 1 then ("", "") else ("(", ")")
  in pref ++ intercalate ", " hsr ++ suff

-- e.g. "String -> String -> String ->"
typeSigList :: HsFun -> String
typeSigList hf = if null (hsparams hf) then "" else intercalate " -> " (map snd $ hsparams hf) ++ " -> "

-- checks whether the given type can be used.
-- The type can be used, if:
-- The type is a standard c type either as a value or
-- as a pointer with a defined meaning, or
-- the type is a pointer to a handle, or
-- the type is a defined enum, or
-- the type is void.
--
-- The meaning (in/out/in+out) of a standard c type is "defined", if
-- the type is not a pointer, or
-- the type is a pointer, and the meaning is given. (not yet implemented)
typeValidMsg :: [String] -> Options -> String -> Maybe String
typeValidMsg enums opts t = validateAll [(t /= "void*", "Void pointer not supported"),
                               (not (isTemplate t), "Template types not supported"),
                               ((any (==(stripNamespace t)) enums || 
                                t == "void" || 
                                isStdType t || 
                                isPtr t > 0), "type " ++ t ++ " is a value of a non-standard type"),
                               (hasDir, "direction for the type " ++ t ++ " has not been defined in the interface file")]
  where validateAll = foldl' validate Nothing
          where validate (Just n) _        = Just n
                validate Nothing  (f, str) = if not f then Just str else Nothing
        hasDir = isPtr t == 0 ||
                   (isPtr t == 1 && (not $ isJust $ cTypeToHs t)) ||
                   t `elem` defaultins opts

addWithFun :: Handle -> HsFun -> IO ()
addWithFun h fun = do
  let fn  = hsfunname fun
      fnw = withFunName fun
      dst = replace "new.*" "delete" fn
      typ = hsFunRetType fun
      pl  = paramList fun
  hPrintf h "%s :: %s(%s -> IO a) -> IO a\n" fnw (typeSigList fun) typ
  hPrintf h "%s %s f = do\n" fnw pl
  hPrintf h "    obj <- %s %s\n" fn pl
  hPrintf h "    res <- f obj\n"
  hPrintf h "    %s obj\n" dst
  hPrintf h "    return res\n\n"

isConstructor :: HsFun -> Bool
isConstructor = constructor . hsfunname

isDestructor :: HsFun -> Bool
isDestructor = destructor . hsfunname

destructor :: String -> Bool
destructor fn = fn =~ ".*_delete$"

constructor :: String -> Bool
constructor fn = fn =~ ".*_new($|_.*$)"

importForeign :: String
importForeign = "import Foreign\nimport Foreign.C.String\nimport Foreign.C.Types\n"

cPrefix :: String
cPrefix = "c_"

-- in: a c type, like "int"
-- out: the haskell conversion function for converting from haskell data type
convFunc :: String -> CConv
convFunc ptype =
  case fromMaybe "" $ cTypeToHs ptype of
    "CChar"   -> CConvFunc "castCharToCChar"
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
convRevFunc :: [String] -> String -> HsConv
convRevFunc enums t
  | fromMaybe "" (cTypeToHs t) == "CBool" = HsConv "toBool"
  | otherwise = 
     let ccf = convFunc t
     in case ccf of
          CConvFunc n -> HsConv n
          _           -> 
            let entype = stripNamespace . correctType . stripExtra . stripConst $ t
            in if entype `elem` enums
                 then HsConv $ printf "cintTo%s" entype
                 else NoHsConv

paramNames :: Int -> [String]
paramNames n = map ('p':) (map show [1..n])

-- printHsType "const char **" = "(Ptr (Ptr CChar))"
printHsType :: [String] -> String -> String
printHsType _     "void" = "()"
printHsType enums t      = 
  case cTypeToHs t of
    Nothing -> hsPointerize (isPtr ct - 1) $ capitalize . fixNamespace enums . correctType . stripConst $ t
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
  intercalate " -> " types ++ " -> " 


