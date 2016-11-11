{-# LANGUAGE CPP #-}
module DeriveMod(deriveMod, deriveSMod, deriveMods, modify)
where

import Language.Haskell.TH
import Data.Char
import Control.Monad.State.Class()
import Control.Monad.State(modify) -- for exporting

{-
 example:

data DataType = Constructor {
    field1 :: String
  , field2 :: Int
  }
$(deriveMods ''DataType)

=>

-- from mkModN'
modifyField1 :: (String -> String) -> DataType -> DataType
modifyField1 f c = c{field1 = f (field1 c)}

-- from toState
sModifyField1 :: (MonadState m) => (String -> String) -> m DataType ()
sModifyField1 f = modify (modifyField1 f)

-- from mkSet
setField1 :: String -> DataType -> DataType
setField1 v c = c{field1 = v}

-- from toState
sSetField1 :: (MonadState m) => String -> m DataType ()
sSetField1 v = modify (setField1 v)

and repeat for field2.

-}

deriveMod :: String -> [Dec]
deriveMod = mkModN' . mkName

-- mkModN' abc => modAbc f c = c{abc = f(abc c)}
mkModN' :: Name -> [Dec]
mkModN' n = 
  let f = mkName "f"
      c = mkName "c"
      m = mkName ("mod" ++ (capitalize (nameBase n)))
  in [FunD m 
         [Clause [VarP f,VarP c] 
                 (NormalB 
                    (RecUpdE (VarE c) 
                             [(n, AppE (VarE f) (AppE (VarE n) (VarE c)))])) []]]

-- mkModM mkModN' abc creates all modFoo functions for datatype abc.
mkModM :: (Name -> [Dec]) -> Name -> Q [Dec]
mkModM crf d = do
  fs <- dToFs d
  let exps = concatMap crf fs
  return $ exps

-- get the fields of a data type.
dToFs d = do
#if (__GLASGOW_HASKELL__ > 710)
  TyConI (DataD _ _ _ _ cons _) <- reify d
#else
  TyConI (DataD   _ _ _ cons _) <- reify d
#endif
  return $ concatMap getF cons

deriveMods :: Name -> Q [Dec]
deriveMods d = do
  fs1 <- mkModM mkModN' d
  let fs2 = deriveSMod fs1
  fs3 <- mkModM mkSet d
  let fs4 = concatMap toState fs3
  return (fs1 ++ fs2 ++ fs3 ++ fs4)

deriveSMod :: [Dec] -> [Dec]
deriveSMod =
  concatMap toState

-- turns a -> b -> b into (MonadState m) => a -> m b ().
toState (FunD fn _) =
  let nn = mkName ('s' : capitalize (nameBase fn))
      f  = mkName "f"
  in [FunD nn
         [Clause [VarP f] 
            (NormalB 
                (AppE (VarE (mkName "modify"))
                      (AppE (VarE fn) (VarE f)))) []]]
toState _ = []

-- get a list of all fields of a constructor.
getF :: Con -> [Name]
getF (RecC _ vars) = let (names, _, _) = unzip3 vars
                     in names
getF _             = []

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

mkSet :: Name -> [Dec]
mkSet n = 
  let v = mkName "v"
      c = mkName "c"
      s = mkName ("set" ++ (capitalize (nameBase n)))
  in [FunD s
        [Clause [VarP v, VarP c]
                 (NormalB
                    (RecUpdE (VarE c)
                             [(n, VarE v)])) []]]

