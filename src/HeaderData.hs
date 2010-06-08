module HeaderData
where

import Safe

type Type = String

data ParamDecl = ParamDecl {
    varname   :: String
  , vartype   :: Type
  , varvalue  :: Maybe String
  , vararray  :: Maybe String
  }
  deriving (Eq, Read, Show)

data Object = FunDecl {
                funname      :: String
              , rettype      :: Type
              , params       :: [ParamDecl]
              , fnnamespace  :: [String]
              , fnvisibility :: Maybe (InheritLevel, String)
              , constclass   :: Bool
              , abstract     :: Bool
              }
            | Namespace String [Object]
            | TypeDef (String, String)
            | ClassDecl {
                classname      :: String
              , classinherits  :: [InheritDecl]
              , classnesting   :: [(InheritLevel, String)]
              , classnamespace :: [String]
              , classobjects   :: [(InheritLevel, Object)]
              }
            | VarDecl ParamDecl (Maybe (InheritLevel, String))
            | EnumDef {
                enumname         :: String
              , enumvalues       :: [EnumVal]
              , enumclassnesting :: [(InheritLevel, String)]
              }
    deriving (Eq, Read, Show)

data InheritDecl = InheritDecl {
    inheritname  :: String
  , inheritlevel :: InheritLevel
  }
  deriving (Eq, Read, Show)

data InheritLevel = Public | Protected | Private
  deriving (Eq, Read, Show, Enum, Bounded)

data EnumVal = EnumVal {
    enumvaluename :: String
  , enumvalue     :: Maybe String
  }
  deriving (Eq, Read, Show)

type Header = [Object]

getFuns :: [Object] -> [Object]
getFuns [] = []
getFuns (o:os) = 
  case o of
    (FunDecl _ _ _ _ _ _ _) -> o : getFuns os
    (Namespace _ os2)       -> getFuns os2 ++ getFuns os
    (ClassDecl _ _ n _ os2) -> 
       case n of
         []              -> getFuns (map snd os2) ++ getFuns os
         ((Public, _):_) -> getFuns (map snd os2) ++ getFuns os
         _               -> getFuns os
    _                       -> getFuns os

getClasses :: [Object] -> [Object]
getClasses [] = []
getClasses (o:os) = 
  case o of
    (Namespace _ os2)       -> getClasses os2 ++ getClasses os
    (ClassDecl _ _ _ _ os2) -> o : getClasses (map snd os2) ++ getClasses os
    _                       -> getClasses os

getObjName :: Object -> String
getObjName (FunDecl n _ _ _ _ _ _) = n
getObjName (Namespace n _ )      = n
getObjName (TypeDef (n, _))      = n
getObjName (ClassDecl n _ _ _ _) = n
getObjName (VarDecl p _)         = varname p
getObjName (EnumDef n _ _)       = n

isAbstractFun :: Object -> Bool
isAbstractFun (FunDecl _ _ _ _ _ _ a) = a
isAbstractFun _                       = False

getUsedFunTypes :: Object -> [String]
getUsedFunTypes (FunDecl _ rt ps _ _ _ _) =
  rt:(map vartype ps)
getUsedFunTypes _ = []

-- return the enum called n, if found.
fetchEnum :: [Object] -> String -> Maybe Object
fetchEnum enums n = headMay $ filter (enumHasName n) enums

-- return the class called n, if found.
fetchClass :: [Object] -> String -> Maybe Object
fetchClass classes n = headMay $ filter (classHasName n) classes

enumHasName :: String -> Object -> Bool
enumHasName n (EnumDef cn _ _) = n == cn
enumHasName _ _                = False

classHasName :: String -> Object -> Bool
classHasName n (ClassDecl cn _ _ _ _) = n == cn
classHasName _ _                      = False

getTypedef :: Object -> Maybe (String, String)
getTypedef (TypeDef t) = Just t
getTypedef _           = Nothing

isEnum :: Object -> Bool
isEnum (EnumDef _ _ _)  = True
isEnum _                = False

getEnum :: Object -> Maybe Object
getEnum o@(EnumDef _ _ _)  = Just o
getEnum _                  = Nothing

getClname :: Object -> String
getClname (FunDecl _ _ _ _ (Just (_, n)) _ _) = n
getClname _                                 = ""


