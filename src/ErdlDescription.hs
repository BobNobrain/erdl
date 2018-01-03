module ErdlDescription
    ( ErdlFile (..)
    , PackageName (..)
    , EntityDescription (..)
    , Column (..)
    , TypeName (..)
    , TypeDescription (..)
    , TypeNameDef (..)
    , TypeBody (..)
    , GeneratingDescriptor (..)
    , GDBody (..)
    , ExternalCall (..)
    , Target (..)

    , makeGeneratingTarget
    ) where

import CommonTypes


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

data TypeDescription = TypeDescription TypeNameDef (Maybe TypeName) (Maybe TypeBody)
    deriving (Show, Eq)

data TypeNameDef = TypeNameDef String [Argument]
    deriving (Show, Eq)

-- name (mb defaultValue)

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
