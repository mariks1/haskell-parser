module Types (Node (..)) where

data Node
  = NString String
  | NNumber Double
  | NBool Bool
  | NNull
  | NArray [Node]
  | NObject [(String, Node)]
  deriving (Show, Eq)
