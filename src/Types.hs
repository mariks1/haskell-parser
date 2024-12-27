module Types where

data Node
  = NString String
  | NNumber Double
  | NBool Bool
  | NNull
  | NArray [Node]
  | NObject [(String, Node)]
  deriving (Show, Eq, Read)

data XML = XElem String [(String, String)] [XML]
         | XText String
         deriving (Show, Eq)