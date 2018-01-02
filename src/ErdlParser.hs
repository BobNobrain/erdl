module ErdlParser
    ( parseErdl
    ) where

import ErdlDescription
import Text.ParserCombinators.Parsec hiding (Column)
import Data.Char (chr)
import Numeric (readHex)


parseErdl :: String -> Either ParseError ErdlFile
parseErdl = parse erdlFile "(input)"


erdlFile :: GenParser Char st ErdlFile
erdlFile = do
    -- _ <- spaces
    -- _ <- string "package"
    -- _ <- spaces
    -- name <- packageName
    _ <- spaces
    ents <- many entity
    _ <- eof
    return $ ErdlFile (PackageName ["name"]) ents

packageName :: GenParser Char st PackageName
packageName = do
    n <- lname
    ns <- rest
    return $ PackageName (n:ns)
    where
        rest :: GenParser Char st [String]
        rest = do
            try $ do
                    _ <- char '.'
                    n <- lname
                    ns <- rest
                    return (n:ns)
            <|> return []

-- variable name literal
lname :: GenParser Char st String
lname = do
    first <- letter
    name <- many $ choice [letter, digit, char '_']
    return (first:name)

entity :: GenParser Char st EntityDescription
entity = do
    anns <- many annotation
    _ <- string "entity"
    _ <- spaces
    entName <- lname
    _ <- spaces
    extends <- optionMaybe $ do
        _ <- string "extends"
        _ <- spaces
        parent <- lname
        _ <- spaces
        return parent
    _ <- char '{'
    _ <- spaces
    columns <- many column
    _ <- char '}'
    _ <- spaces
    return $ EntityDescription entName extends columns anns

annotation :: GenParser Char st Annotation
annotation = do
    _ <- char '@'
    name <- lname
    params <- parameterList
    _ <- spaces
    return $ Annotation name params

parameterList :: GenParser Char st [Parameter]
parameterList = do
    try $ do
            _ <- char '('
            _ <- spaces
            params <- sepBy param (char ',')
            _ <- char ')'
            _ <- spaces
            return params
    <|> return []
    where
        param :: GenParser Char st Parameter
        param = do
            _ <- spaces
            p <- parameter
            _ <- spaces
            return p

parameter :: GenParser Char st Parameter
parameter = choice [numParam, strParam, namedParamOrFlag] where
    numParam :: GenParser Char st Parameter
    numParam = do
        n <- lnum
        return $ PlainParameter $ pv n where
            pv (LInt i) = PVInt i
            pv (LDouble d) = PVDouble d

    strParam :: GenParser Char st Parameter
    strParam = do
        s <- lstr
        return $ PlainParameter $ PVString s

    namedParamOrFlag :: GenParser Char st Parameter
    namedParamOrFlag = do
        name <- lname
        case name of "not" -> do
                                _ <- spaces
                                flagName <- lname
                                return $ FlagParameter flagName False
                     "true" -> return $ PlainParameter $ PVBool True
                     "false" -> return $ PlainParameter $ PVBool False
                     _ -> try (readRest name) <|> (return $ FlagParameter name True)

    readRest :: String -> GenParser Char st Parameter
    readRest name = do
        _ <- spaces
        _ <- char '='
        PlainParameter pv <- choice [numParam, strParam]
        return $ NamedParameter name pv


data LNum
    = LInt Integer
    | LDouble Double

-- numeric literal
lnum :: GenParser Char st LNum
lnum = do
    isNegative <- optionalMinus
    wholePart <- wholePartRule
    hasFractionalCheck <- optionMaybe $ char '.'
    let hasFractional = case hasFractionalCheck of Nothing -> False
                                                   Just '.' -> True

    fractionalPart <- if hasFractional then fracPartRule else return 0.0

    hasExpCheck <- optionMaybe $ choice [char 'e', char 'E']
    let hasExp = case hasExpCheck of Nothing -> False
                                     Just _ -> True

    exponentialPart <- if hasExp then expPartRule else return 0.0

    return $ constructNumber isNegative (hasFractional && hasExp) wholePart fractionalPart exponentialPart
    where
        constructNumber :: Bool -> Bool -> Integer -> Double -> Double -> LNum
        constructNumber isNegative True wholePart fractionalPart exponentialPart = LDouble n where
            n = if isNegative then (-n1) else n1
            n1 = n2 * (10.0 ** exponentialPart)
            n2 :: Double
            n2 = fractionalPart + fromInteger wholePart

        constructNumber isNegative False wholePart _ _ = LInt n where
            n = if isNegative then (-wholePart) else wholePart

        optionalMinus = do
            r <- optionMaybe $ char '-'
            case r of Nothing -> return False
                      Just '-' -> return True

        wholePartRule = do
            first <- choice [char '0', oneOf ['1'..'9']]
            rest <- many digit
            return $ read (first:rest)

        fracPartRule = do
            digits <- many digit
            return $ read $ "0." ++ digits

        expPartRule = do
            signMb <- optionMaybe $ choice [char '-', char '+']
            digits <- many digit
            return $ ct signMb digits where
                ct :: Maybe Char -> String -> Double
                ct Nothing ds = read ds
                ct (Just '-') ds = -read ds
                ct (Just '+') ds = read ds

-- string literal, wrapped either in '\'' or '"'
lstr :: GenParser Char st String
lstr = do
    c <- oneOf "'\""
    strC <- strContent c
    _ <- char c
    return strC
    where
        unescapedChar :: Char -> GenParser Char st Char
        unescapedChar c = noneOf [c, '\\']

        escapedChar :: Char -> GenParser Char st Char
        escapedChar ch = do
            _ <- char '\\'
            c <- oneOf [ch, '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
            case c of 'u' -> do
                                code <- count 4 $ oneOf $ ['0'..'9'] ++ ['A'..'F']
                                return $ (chr . fst . head . readHex) code
                      _ -> return $ cvt c
                           where
                               cvt :: Char -> Char
                               cvt 'b' = '\b'
                               cvt 'f' = '\f'
                               cvt 'n' = '\n'
                               cvt 'r' = '\r'
                               cvt 't' = '\t'
                               cvt c = c

        strContent :: Char -> GenParser Char st String
        strContent c = many $ choice [escapedChar c, unescapedChar c]

column :: GenParser Char st Column
column = do
    colName <- lname
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    colType <- typeName
    via <- optionMaybe viaRef
    _ <- spaces
    return $ Column colName colType via

typeName :: GenParser Char st TypeName
typeName = do
    name <- lname
    mul <- option False hasBrackets
    _ <- spaces
    ps <- parameterList
    return $ TypeName name mul ps
    where
        hasBrackets :: GenParser Char st Bool
        hasBrackets = do
            _ <- spaces
            _ <- char '['
            _ <- spaces
            _ <- char ']'
            return True

viaRef :: GenParser Char st String
viaRef = do
    _ <- string "via"
    _ <- spaces
    name <- lname
    return name

