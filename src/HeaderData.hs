module HeaderData
where

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
              , abstract     :: Bool
              }
            | Namespace String [Object]
            | TypeDef (String, String)
            | ClassDecl {
                classname      :: String
              , classinherits  :: [InheritDecl]
              , classnesting   :: [(InheritLevel, String)]
              , classnamespace :: [String]
              , classobjects   :: [Object]
              }
            | VarDecl ParamDecl (Maybe (InheritLevel, String))
            | EnumDef String [EnumVal]
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


