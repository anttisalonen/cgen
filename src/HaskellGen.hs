module HaskellGen (haskellGen)
where

import System.IO
import Control.Monad
import qualified Data.Set as S

import HeaderData
import CppUtils

haskellGen :: FilePath -> [(FilePath, [Object])] -> IO ()
haskellGen outdir objs = do
    let funs     = getFuns (concat (map snd objs))
        alltypes = getAllTypesWithPtr funs
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

removeNamespace :: String -> String
removeNamespace = map (\c -> if c == ':' then '_' else c)

