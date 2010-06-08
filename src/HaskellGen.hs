module HaskellGen (haskellGen)
where

import Data.List
import Text.Printf
import System.IO
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
    forM_ hstypes $ \t -> do
        hPrintf stderr "newtype %s = %s (Ptr %s)\n" t t t

    forM_ funs $ \(file, filefuns) ->
        forM_ filefuns $ \fun -> 
            if (all (\t -> isStdType (clearType t) || hsType t `elem` hstypes || t == "void") (getUsedFunTypes fun)) 
              then
                hPrintf stderr (hsFFIFun file fun)
              else
                hPrintf stderr "Function %s discarded:\n\t%s\n" (getObjName fun) (hsFFIFun file fun)

clearType = stripPtr . correctType . stripConst

hsFFIFun :: String -> Object -> String
hsFFIFun file fun = printf "foreign import ccall \"%s %s\"  c_%s :: %sIO %s\n" 
                          file (getObjName fun) (getObjName fun)
                          (hsParams (params fun))
                          (if isStdType (clearType $ rettype fun)
                             then cTypeToHs (correctType . stripConst $ rettype fun)
                             else (printHsType (rettype fun)))

hsType :: String -> String
hsType "void" = "()"
hsType t      = capitalize . stripPtr . removeNamespace . correctType . stripConst $ t

printHsType :: String -> String
printHsType "void" = "()"
printHsType t      = hsPointerize $ capitalize . removeNamespace . correctType . stripConst $ t

hsPointerize t = 
  let numptrs = isPtr t
  in concat (replicate numptrs "(Ptr ") ++ (stripPtr t) ++ concat (replicate numptrs ")")

-- TODO: check
cTypeToHs "float" = "CFloat"
cTypeToHs "double" = "CDouble"
cTypeToHs "char" = "CChar"
cTypeToHs "int" = "CInt"
cTypeToHs "unsigned int" = "CUInt"
cTypeToHs "signed int" = "CInt"
cTypeToHs "long" = "CLong"
cTypeToHs "unsigned long" = "CULong"
cTypeToHs "signed long" = "CLong"
cTypeToHs "bool" = "CBool"
cTypeToHs "short" = "CShort"
cTypeToHs "unsigned short" = "CUShort"
cTypeToHs "signed short" = "CShort"
cTypeToHs "unsigned" = "CInt"
cTypeToHs "long long" = "CLongLong"
cTypeToHs "unsigned long long" = "CULongLong"
cTypeToHs "int8_t" = "CChar"
cTypeToHs "uint8_t" = "CByte"
cTypeToHs "int16_t" = "CShort"
cTypeToHs "uint16_t" = "CUShort"
cTypeToHs "int32_t" = "CLong"
cTypeToHs "uint32_t" = "CULong"
cTypeToHs "int64_t" = "CLongLong"
cTypeToHs "uint64_t" = "CULongLong"
cTypeToHs "size_t" = "CSize"
cTypeToHs "uint8" = "CByte"
cTypeToHs "uint16" = "CUShort"
cTypeToHs "uint32" = "CULong"
cTypeToHs "uint64" = "CULongLong"
cTypeToHs _ = error "Invalid C type - the function cTypeToHs doesn't match isStdType"

hsParams :: [ParamDecl] -> String
hsParams [] = ""
hsParams ps = 
  let types = map vartype ps
  in intercalate " -> " (map hsType types) ++ " -> " 

removeNamespace :: String -> String
removeNamespace = map (\c -> if c == ':' then '_' else c)

