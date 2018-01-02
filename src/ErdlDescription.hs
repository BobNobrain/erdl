module ErdlDescription
    ( EntityDescription (..)
    , Column (..)
    , TypeName (..)
    , Parameter (..)
    , ParameterValue (..)
    , Annotation (..)
    ) where

data EntityDescription = EntityDescription String [Column] [Annotation]

data Column = Column String TypeName

data TypeName = TypeName String [Parameter]

data Parameter
    = NamedParameter String ParameterValue
    | FlagParameter String Bool
    | PlainParameter ParameterValue

data ParameterValue
    = PVInt Int
    | PVString String
    | PVFloat Float
    | PVBool Bool

data Annotation = Annotation String [Parameter]
