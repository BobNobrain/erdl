module CommonTypes
    ( Annotation (..)
    , Parameter (..)
    , ParameterValue (..)
    , Argument (..)
    , ArgumentType (..)

    , isCompatible
    , getValueType
    ) where

data Annotation = Annotation String [Parameter]
    deriving (Show, Eq)

data Parameter
    = NamedParameter String ParameterValue
    | FlagParameter String Bool
    | PlainParameter ParameterValue
    deriving (Show, Eq)

data ParameterValue
    = PVInt Integer
    | PVString String
    | PVDouble Double
    | PVBool Bool
    | PVBound Argument
    deriving (Show, Eq)

data Argument = Argument String ArgumentType (Maybe ParameterValue)
    deriving (Show, Eq)

data ArgumentType = TInt | TString | TDouble | TBool | TFlag | TUnknown
    deriving (Show, Eq)

isCompatible :: ArgumentType -> ParameterValue -> Bool
isCompatible TInt (PVInt _) = True
isCompatible TString (PVString _) = True
isCompatible TDouble (PVDouble _) = True
isCompatible TBool (PVBool _) = True
isCompatible t (PVBound (Argument _ u _)) = t == u
isCompatible _ _ = False

getValueType :: ParameterValue -> ArgumentType
getValueType (PVInt _) = TInt
getValueType (PVDouble _) = TDouble
getValueType (PVBool _) = TBool
getValueType (PVString _) = TString
getValueType (PVBound (Argument _ t _)) = t
-- redundant:
-- getValueType _ = TUnknown

