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
    entries <- many $ choice [config, entryPoints, exports]
    eof
    let cfgs = listConfigurations entries
    let epts = listEps entries
    let exps = listExports entries
    return $ PackageFileDescription pname epts cfgs (concat exps)


data PkgEntry
    = PEConfig Configuration
    | PEEntryPoints EntryPoints
    | PEExports [String]
    deriving (Show, Eq)

listConfigurations :: [PkgEntry] -> [Configuration]
listConfigurations = (map extract . filter f) where
    f (PEConfig _) = True
    f _ = False
    extract (PEConfig c) = c

listEps :: [PkgEntry] -> [EntryPoints]
listEps = (map extract . filter f) where
    f (PEEntryPoints _) = True
    f _ = False
    extract (PEEntryPoints e) = e

listExports :: [PkgEntry] -> [[String]]
listExports = (map extract . filter f) where
    f (PEExports _) = True
    f _ = False
    extract (PEExports s) = s


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
    props <- many pkgProperty
    char '}'
    spacesOrComments
    return $ PEEntryPoints $ mkEp props ans
    where
        mkEp props ans = EntryPoints ents api extr extn ans where
            l :: [(String, PackageName)] -> String -> Maybe PackageName
            l ((n, p):ps) name = if name == n then Just p else l ps name
            l [] _ = Nothing
            l' = l props
            ents = l' "entities"
            api = l' "api"
            extr = l' "external"
            extn = l' "extensions"


exports :: GenParser Char st PkgEntry
exports = do
    string "exports"
    es <- sepBy spacedLname (char ',')
    return $ PEExports es
    where
        spacedLname = do
            spacesOrComments
            s <- choice [string "*", lname]
            spacesOrComments
            return s


data Property
    = PlainProp String ParameterValue
    | NestedProp String [Property]
    deriving (Show, Eq)

propertyToField :: Property -> Field
propertyToField (PlainProp name val) = PlainField name val
propertyToField (NestedProp name vs) = NestedField name (map propertyToField vs)

lookupProperty :: String -> [Property] -> Maybe Property
lookupProperty name ((PlainProp p v):ps) = if name == p then return (PlainProp p v) else lookupProperty name ps
lookupProperty name ((NestedProp p vs):ps) = if name == p then return (NestedProp p vs) else lookupProperty name ps
lookupProperty _ [] = Nothing

property :: GenParser Char st Property
property = do
    name <- lname
    spacesOrComments
    if name == "not" then do
        flagName <- lname
        spacesOrComments
        return $ PlainProp flagName (PVBool False)
    else do
        result <- option (PlainProp name (PVBool True)) (propValue name)
        return result
    where
        propValue name = do
            char ':'
            spacesOrComments
            nested <- option False (char '{' >> return True)
            if nested then do
                spacesOrComments
                children <- many property
                char '}'
                spacesOrComments
                return $ NestedProp name children
            else do
                pv <- choice [numVal, strVal, boolVal, noneVal]
                spacesOrComments
                return $ PlainProp name pv


pkgProperty :: GenParser Char st (String, PackageName)
pkgProperty = do
    name <- lname
    spacesOrComments
    char ':'
    spacesOrComments
    pkg <- inlinePackageName
    spacesOrComments
    return (name, pkg)
