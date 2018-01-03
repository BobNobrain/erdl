module PackageDescription
    ( PackageFileDescription
    , Configuration
    , EntryPoints (..)
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

lookupConfigurationField :: Configuration -> String -> Maybe Field
lookupConfigurationField (Configuration _ fs) = lookup fs where
    lookup :: [Field] -> String -> Maybe Field
    lookup ((PlainField fn pv):fs) fname = l fn fname fs $ PlainField fn pv
    lookup ((NestedField fn fvs):fs) fname = l fn fname fs $ NestedField fn fvs
    lookup [] _ = Nothing

    l :: String -> String -> [Field] -> Field -> Maybe Field
    l fn fname fs r =
        if fn == fname then
            return r
        else
            lookup fs fname
