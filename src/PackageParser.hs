module PackageParser
    ( parsePackageFile
    ) where

import CommonTypes
import CommonRules
import PackageDescription
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)


parsePackageFile :: String -> Either ParseError PackageFileDescription
parsePackageFile = parse packageFile "(input)"

packageFile :: GenParser Char st PackageFileDescription
packageFile = do
    spacesOrComments
    pname <- packageName
    entries <- many $ choise [config, entryPoints]
    let cfgs = listConfigurations entries
    let epts = listEps entries
    return $ PackageFileDescription pname epts cfgs

data PkgEntry
    = PEConfig Configuration
    | PEEntryPoints EntryPoints
    deriving (Show, Eq)

listConfigurations :: [PkgEntry] -> [Configuration]
listConfigurations = (map extract . filter f) where
    f (PEConfig _) = True
    f _ = False
    extract (PEConfig c) = c

listEps :: [PkgEntry] -> [Configuration]
listEps = (map extract . filter f) where
    f (PEEntryPoints _) = True
    f _ = False
    extract (PEEntryPoints e) = e

config :: GenParser Char st PkgEntry
config = do
    ans <- many annotation
    string "config"
    spacesOrComments
    char '{'
    spacesOrComments
    props <- many property
    char '}'
    spacesOrComments
    return $ PEConfig $ Configuration ans (map propertyToField props)


entryPoints :: GenParser Char st PkgEntry
entryPoints = do
    ans <- many annotation
    string "use"
    spacesOrComments
    char '{'
    spacesOrComments
    props <- many property
    char '}'
    spacesOrComments
    return $ PEEntryPoints $ EntryPoints ents api extr extn
    where
        ents = lookupProperty "entities"


data Property
    = PlainProp String ParameterValue
    | PkgNameProp PackageName
    | NestedProp String [Property]
    deriving (Show, Eq)

propertyToField :: Property -> Field
propertyToField (PlainProp name val) = PlainField name val
propertyToField (NestedProp name vs) = NestedField name (map propertyToField vs)

lookupProperty :: String -> [Property] -> Maybe Property
lookupProperty name ((PlainProp p v):ps) = if name == p then return (PlainProp p v) else lookupProperty ps
lookupProperty name ((NestedProp p vs):ps) = if name == p then return (NestedProp p vs) else lookupProperty ps
lookupProperty _ [] = Nothing

property :: GenParser Char st Property
