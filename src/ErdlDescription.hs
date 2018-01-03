module ErdlDescription
    ( ErdlFile (..)
    , PackageName (..)
    , EntityDescription (..)
    , Column (..)
    , TypeName (..)
    , Parameter (..)
    , ParameterValue (..)
    , Annotation (..)
    , TypeDescription (..)
    , TypeNameDef (..)
    , Argument (..)
    , ArgumentType (..)
    , TypeBody (..)
    , GeneratingDescriptor (..)
    , GDBody (..)
    , ExternalCall (..)
    , Target (..)

    , makeGeneratingTarget
    , isCompatible
    , getValueType
    ) where

-- TODO: also add [TypeDescription], ConfigurationDescription and others
data ErdlFile = ErdlFile PackageName [EntityDescription] [TypeDescription]
    deriving (Show, Eq)

data PackageName = PackageName [String]
    deriving (Show, Eq)

-- name parent columns annotations
data EntityDescription = EntityDescription String (Maybe String) [Column] [Annotation]
    deriving (Show, Eq)

-- name type viaRef
data Column = Column String TypeName (Maybe String)
    deriving (Show, Eq)

-- name isMultiple parameters
data TypeName = TypeName String Bool [Parameter]
    deriving (Show, Eq)

data Parameter
    = NamedParameter String ParameterValue
    | FlagParameter String Bool
    | PlainParameter ParameterValue
    deriving (Show, Eq)

data ParameterValue
    = PVInt Integer
    | PVString String
    | PVDouble Double
    | PVBool Bool
    | PVBound Argument
    deriving (Show, Eq)

data Annotation = Annotation String [Parameter]
    deriving (Show, Eq)

data TypeDescription = TypeDescription TypeNameDef (Maybe TypeName) (Maybe TypeBody)
    deriving (Show, Eq)

data TypeNameDef = TypeNameDef String [Argument]
    deriving (Show, Eq)

-- name (mb defaultValue)
data Argument = Argument String ArgumentType (Maybe ParameterValue)
    deriving (Show, Eq)

data TypeBody = TypeBody [GeneratingDescriptor]
    deriving (Show, Eq)

data GeneratingDescriptor = GeneratingDescriptor Target GDBody
    deriving (Show, Eq)

data Target
    = TargetORM (Maybe String)
    | TargetSQL (Maybe String)
    | UnknownTarget String
    deriving (Show, Eq)

makeGeneratingTarget :: String -> Maybe String -> Target
makeGeneratingTarget "orm" mb = TargetORM mb
makeGeneratingTarget "sql" mb = TargetSQL mb
makeGeneratingTarget s _ = UnknownTarget s

data GDBody
    = GDBValue String
    | GDBExternal ExternalCall
    deriving (Show, Eq)

data ExternalCall = ExternalCall String [Parameter]
    deriving (Show, Eq)

data ArgumentType = TInt | TString | TDouble | TBool | TFlag | TUnknown
    deriving (Show, Eq)

isCompatible :: ArgumentType -> ParameterValue -> Bool
isCompatible TInt (PVInt _) = True
isCompatible TString (PVString _) = True
isCompatible TDouble (PVDouble _) = True
isCompatible TBool (PVBool _) = True
isCompatible t (PVBound (Argument _ u _)) = t == u
isCompatible _ _ = False

getValueType :: ParameterValue -> ArgumentType
getValueType (PVInt _) = TInt
getValueType (PVDouble _) = TDouble
getValueType (PVBool _) = TBool
getValueType (PVString _) = TString
getValueType (PVBound (Argument _ t _)) = t
-- redundant:
-- getValueType _ = TUnknown
