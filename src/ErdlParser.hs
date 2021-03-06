module ErdlParser
    ( parseErdl
    ) where

import CommonRules
import CommonTypes
import ErdlDescription
import Text.ParserCombinators.Parsec hiding (Column)
import Text.Parsec.Char (endOfLine)


parseErdl :: String -> Either ParseError ErdlFile
parseErdl = parse erdlFile "(input)"


data ErdlFileEntry
    = EEntity EntityDescription
    | EType TypeDescription

listEntities :: [ErdlFileEntry] -> [EntityDescription]
listEntities es = (map extract . filter f) es where
    f (EEntity _) = True
    f _ = False
    extract (EEntity e) = e

listTypes :: [ErdlFileEntry] -> [TypeDescription]
listTypes ts = (map extract . filter f) ts where
    f (EType _) = True
    f _ = False
    extract (EType t) = t

erdlFile :: GenParser Char st ErdlFile
erdlFile = do
    spacesOrComments
    name <- option (PackageName ["."]) packageName
    spacesOrComments
    entries <- many $ choice [entity, typeDef]
    eof
    return $ ErdlFile name (listEntities entries) (listTypes entries)

entity :: GenParser Char st ErdlFileEntry
entity = do
    anns <- many annotation
    string "entity"
    spacesOrComments
    entName <- lname
    spacesOrComments
    extends <- optionMaybe $ do
        string "extends"
        spacesOrComments
        parent <- lname
        spacesOrComments
        return parent
    char '{'
    spacesOrComments
    columns <- many column
    char '}'
    spacesOrComments
    return $ EEntity $ EntityDescription entName extends columns anns

column :: GenParser Char st Column
column = do
    colName <- lname
    spacesOrComments
    char ':'
    spacesOrComments
    colType <- typeName
    via <- optionMaybe viaRef
    spacesOrComments
    return $ Column colName colType via

typeName :: GenParser Char st TypeName
typeName = do
    name <- lname
    spacesOrComments
    mul <- option False hasBrackets
    ps <- option [] parameterList
    return $ TypeName name mul ps
    where
        hasBrackets :: GenParser Char st Bool
        hasBrackets = do
            char '['
            spacesOrComments
            char ']'
            spacesOrComments
            return True

viaRef :: GenParser Char st String
viaRef = do
    string "via"
    spacesOrComments
    name <- lname
    return name

typeDef :: GenParser Char st ErdlFileEntry
typeDef = do
    string "type"
    spacesOrComments
    name <- lname
    spacesOrComments
    args <- option [] argumentsList
    overrides <- optionMaybe typeOverride
    extension <- optionMaybe typeExtension
    return $ EType $ TypeDescription (TypeNameDef name args) overrides extension
    where
        typeOverride = do
            char '='
            spacesOrComments
            typeName 

typeExtension :: GenParser Char st TypeBody
typeExtension = do
    char '{'
    spacesOrComments
    generators <- many generatorDescription
    char '}'
    spacesOrComments
    return $ TypeBody generators

generatorDescription :: GenParser Char st GeneratingDescriptor
generatorDescription = do
    target <- lname
    spacesOrComments
    subtype <- optionMaybe generatorSubtype
    char ':'
    spacesOrComments
    gdb <- generatorDescriptionBody
    return $ GeneratingDescriptor (makeGeneratingTarget target subtype) gdb

generatorDescriptionBody :: GenParser Char st GDBody
generatorDescriptionBody = do
    isCall <- option False (string "call" >> return True)
    if isCall then do
        spacesOrComments
        fnName <- lname
        spacesOrComments
        params <- option [] parameterList
        return $ GDBExternal $ ExternalCall fnName params
    else do
        s <- lstr
        spacesOrComments
        return $ GDBValue s

generatorSubtype :: GenParser Char st String
generatorSubtype = do
    char '('
    spacesOrComments
    result <- lstr
    spacesOrComments
    char ')'
    spacesOrComments
    return result

argumentsList :: GenParser Char st [Argument]
argumentsList = do
    char '('
    args <- sepBy argument (char ',')
    char ')'
    spacesOrComments
    return args
    where
        argument :: GenParser Char st Argument
        argument = do
            spacesOrComments
            name <- lname
            spacesOrComments
            explicitType <- option TUnknown argTypedef
            defVal <- optionMaybe argDefVal
            let argType = defineArgType defVal explicitType
            return $ Argument name argType defVal

        defineArgType :: (Maybe ParameterValue) -> ArgumentType -> ArgumentType
        defineArgType (Just pv) t = if dt == t then t else TUnknown where
            dt = getValueType pv
        defineArgType Nothing t = t

        argTypedef :: GenParser Char st ArgumentType
        argTypedef = do
            char ':'
            spacesOrComments
            t <- choice [string "string", string "int", string "double", string "bool", string "flag"]
            spacesOrComments
            return $ makeTypeFromString t

        makeTypeFromString :: String -> ArgumentType
        makeTypeFromString "string" = TString
        makeTypeFromString "int" = TInt
        makeTypeFromString "double" = TDouble
        makeTypeFromString "bool" = TBool
        makeTypeFromString "flag" = TBool
        makeTypeFromString _ = TUnknown

        argDefVal :: GenParser Char st ParameterValue
        argDefVal = do
            char '='
            spacesOrComments
            pv <- choice [numVal, strVal, boolVal]
            spacesOrComments
            return pv

        boolVal :: GenParser Char st ParameterValue
        boolVal = try (string "true"  >> return (PVBool True))
              <|> try (string "false" >> return (PVBool False))
