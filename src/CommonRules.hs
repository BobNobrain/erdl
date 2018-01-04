module CommonRules
    ( lname
    , lnum
    , LNum (..)
    , lstr
    , lbool

    , numVal
    , strVal
    , boolVal
    , noneVal

    , annotation
    , inlinePackageName
    , packageName
    , parameterList
    , spacesOrComments
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Data.Char (chr)
import Numeric (readHex)
import CommonTypes


packageName :: GenParser Char st PackageName
packageName = do
    string "package"
    spacesOrComments
    inlinePackageName

inlinePackageName :: GenParser Char st PackageName
inlinePackageName = do
    n <- choice [string "~", lname]
    ns <- rest
    spacesOrComments
    return $ PackageName (n:ns)
    where
        rest :: GenParser Char st [String]
        rest = do
            try $ do
                    char '.'
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

data LNum
    = LInt Integer
    | LDouble Double


-- numeric literal, either integer or double
lnum :: GenParser Char st LNum
lnum = do
    isNegative <- optionalMinus
    wholePart <- wholePartRule
    hasFractional <- option False (char '.' >> return True)
    hasExp <- option False (choice [char 'e', char 'E'] >> return True)
    
    fractionalPart <- if hasFractional then fracPartRule else return 0.0
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
    char c
    return strC
    where
        unescapedChar :: Char -> GenParser Char st Char
        unescapedChar c = noneOf [c, '\\']

        escapedChar :: Char -> GenParser Char st Char
        escapedChar ch = do
            char '\\'
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


lbool :: GenParser Char st Bool
lbool = try (do
        v <- lname
        case v of "true" -> return True
                  "false" -> return False
                  _ -> fail "This is not a boolean value!"
    )


spacesOrComments :: GenParser Char st ()
spacesOrComments = do
    spaces
    r <- option False comment
    if r then spacesOrComments else return ()


comment :: GenParser Char st Bool
comment = do
        try (do
                string "//"
                manyTill anyChar endOfLine
                return True
            )
    <|> try ( do
                string "/*"
                manyTill anyChar (string "*/")
                return True
            )
    <|> return False


annotation :: GenParser Char st Annotation
annotation = do
    char '@'
    name <- lname
    params <- option [] parameterList
    spacesOrComments
    return $ Annotation name params


parameterList :: GenParser Char st [Parameter]
parameterList = do
    char '('
    spacesOrComments
    params <- sepBy param (char ',')
    char ')'
    spacesOrComments
    return params
    where
        param :: GenParser Char st Parameter
        param = do
            spacesOrComments
            p <- parameter
            spacesOrComments
            return p


parameter :: GenParser Char st Parameter
parameter = choice [numParam, strParam, namedParamOrFlag] where
    numParam = do
        pv <- numVal
        return $ PlainParameter pv

    strParam = do
        pv <- strVal
        return $ PlainParameter pv

    namedParamOrFlag :: GenParser Char st Parameter
    namedParamOrFlag = do
        name <- lname
        -- TODO: refactor
        case name of "not" -> do
                                spacesOrComments
                                flagName <- lname
                                return $ FlagParameter flagName False
                     "true" -> return $ PlainParameter $ PVBool True
                     "false" -> return $ PlainParameter $ PVBool False
                     "none" -> return $ PlainParameter $ PVNone
                     _ -> try (readRest name) <|> (return $ FlagParameter name True)

    readRest :: String -> GenParser Char st Parameter
    readRest name = do
        spacesOrComments
        char '='
        spacesOrComments
        pv <- choice [numVal, strVal]
        return $ NamedParameter name pv


numVal :: GenParser Char st ParameterValue
numVal = do
    n <- lnum
    return $ pv n where
        pv (LInt i) = PVInt i
        pv (LDouble d) = PVDouble d


strVal :: GenParser Char st ParameterValue
strVal = do
    s <- lstr
    return $ PVString s

boolVal :: GenParser Char st ParameterValue
boolVal = do
    b <- lbool
    return $ PVBool b

noneVal :: GenParser Char st ParameterValue
noneVal = try (do
        n <- lname
        if n == "none" then
            return PVNone
        else
            fail "This is not a 'none' value!"
    )
