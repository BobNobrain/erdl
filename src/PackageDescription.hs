module PackageDescription
    ( PackageFileDescription
    , Configuration
    , EntryPoints (..)

    , entitiesEntryPoint
    , apiEntryPoint
    , externalEntryPoint
    , extensionsEntryPoint
    ) where

import CommonTypes

data PackageFileDescription = PackageFileDescription PackageName [EntryPoints] [Configuration]
    deriving (Show, Eq)

data EntryPoints = EntryPoints { entitiesEntryPoint :: Maybe PackageName
                               , apiEntryPoint :: Maybe PackageName
                               , externalEntryPoint :: Maybe PackageName
                               , extensionsEntryPoint :: Maybe PackageName
                               , annotations :: [Annotation]
                               } deriving (Show, Eq)

data Configuration = Configuration [Annotation] [Field]
    deriving (Show, Eq)

data Field
    = PlainField String ParameterValue
    | NestedField String [Field]
    deriving (Show, Eq)

lookupConfigurationField :: Configuration -> String -> Maybe ParameterValue
lookupConfigurationField (Configuration _ fs) = lookup fs where
    lookup :: [Field] -> String -> Maybe ParameterValue
    lookup ((Field fn pv):fs) fname =
        if fn == fname then
            return pv
        else
            lookup fs fname
    lookup [] _ = Nothing
