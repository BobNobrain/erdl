module CommonRules
    ( lname
    , lstr
    , lnum
    , LNum (..)
    , lbool
    , spacesOrComments
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Data.Char (chr)
import Numeric (readHex)


-- variable name literal
lname :: GenParser Char st String
lname = do
    first <- letter
    name <- many $ choice [letter, digit, char '_']
    return (first:name)

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

lbool :: GenParser Char st Bool
lbool = try (string "true"  >> return True)
    <|> try (string "false" >> return False)

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
