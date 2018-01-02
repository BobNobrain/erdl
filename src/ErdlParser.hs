module ErdlParser
    ( parseErdl
    ) where

import ErdlDescription
import Text.ParserCombinators.Parsec


data ErdlToken
    = CommentStart
    | CommentEnd
    | AnnotationStart
    | BodyStart
    | BodyEnd
    | ArgsStart
    | ArgsEnd
    | KWConfig
    | KWEntity
    | KWExports
    | KWExtends
    | KWNot
    | KWPackage
    | KWType
    | KWVia
    | Colon
    | Equals
    | LineBreak
    | LName String
    | LString String
    | LInt Int
    | LFloat Float
    | LBool Bool

parseErdl :: String -> Either ParseError [Entity]
parseErdl input = parse' tokens
    where
        tokens = splitToTokens input
        parse' (Left err) = Left err
        parse' (Right ts) = processTokens ts

splitToTokens :: String -> Either ParseError [ErdlToken]
splitToTokens = parse erdlFile "(input)"

processTokens :: [ErdlToken] -> Either ParseError [Entity]
processTokens = parse erdlContent "(input)"
