module ErdlDescription
    ( ErdlFile (..)
    , PackageName (..)
    , EntityDescription (..)
    , Column (..)
    , TypeName (..)
    , Parameter (..)
    , ParameterValue (..)
    , Annotation (..)
    ) where

-- TODO: also add [TypeDescription], ConfigurationDescription and others
data ErdlFile = ErdlFile PackageName [EntityDescription]
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
    deriving (Show, Eq)

data Annotation = Annotation String [Parameter]
    deriving (Show, Eq)
