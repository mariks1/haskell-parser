module Converter (nodeToJSON, prettyNodeToXML, nodeToYAML) where

import Types

nodeToJSON :: Node -> String
nodeToJSON = formatNode 0

formatNode :: Int -> Node -> String
formatNode _ (NString s) = show s
formatNode _ (NNumber n) = show n
formatNode _ (NBool True) = "true"
formatNode _ (NBool False) = "false"
formatNode _ NNull = "null"
formatNode indent (NArray nodes) =
  "[\n"
    ++ formatArray (indent + 2) nodes
    ++ "\n"
    ++ replicate indent ' '
    ++ "]"
formatNode indent (NObject kvs) =
  "{\n"
    ++ formatObject (indent + 2) kvs
    ++ "\n"
    ++ replicate indent ' '
    ++ "}"

formatArray :: Int -> [Node] -> String
formatArray indent [] = replicate indent ' '
formatArray indent nodes =
  concatMap (\node -> replicate indent ' ' ++ formatNode indent node ++ ",\n") (init nodes)
    ++ replicate indent ' '
    ++ formatNode indent (last nodes)

formatObject :: Int -> [(String, Node)] -> String
formatObject indent [] = replicate indent ' '
formatObject indent kvs =
  concatMap
    ( \(key, value) ->
        replicate indent ' ' ++ "\"" ++ key ++ "\": " ++ formatNode indent value ++ ",\n"
    )
    (init kvs)
    ++ replicate indent ' '
    ++ "\""
    ++ fst (last kvs)
    ++ "\": "
    ++ formatNode indent (snd (last kvs))

prettyNodeToXML :: Node -> String
prettyNodeToXML node =
  "<root>\n"
    ++ prettyNodeToXML' node 2
    ++ "</root>\n"

prettyNodeToXML' :: Node -> Int -> String
prettyNodeToXML' (NString text) indent =
  replicate indent ' ' ++ escape text
prettyNodeToXML' (NNumber num) indent =
  replicate indent ' ' ++ show num
prettyNodeToXML' (NBool True) indent =
  replicate indent ' ' ++ "true"
prettyNodeToXML' (NBool False) indent =
  replicate indent ' ' ++ "false"
prettyNodeToXML' NNull indent =
  replicate indent ' '
prettyNodeToXML' (NArray nodes) indent =
  replicate indent ' '
    ++ "<array>\n"
    ++ concatMap (renderItem (indent + 2)) nodes
    ++ replicate indent ' '
    ++ "</array>\n"
  where
    renderItem itemIndent node =
      replicate itemIndent ' '
        ++ "<item>\n"
        ++ prettyNodeToXML' node (itemIndent + 2)
        ++ "\n"
        ++ replicate itemIndent ' '
        ++ "</item>\n"
prettyNodeToXML' (NObject fields) indent =
  concatMap
    ( \(key, value) ->
        replicate indent ' '
          ++ "<"
          ++ escape key
          ++ ">\n"
          ++ prettyNodeToXML' value (indent + 2)
          ++ "\n"
          ++ replicate indent ' '
          ++ "</"
          ++ escape key
          ++ ">\n"
    )
    fields

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar c = [c]

-- | Основная функция конвертации Node в YAML
nodeToYAML :: Node -> String
nodeToYAML = nodeToYAML' 0

nodeToYAML' :: Int -> Node -> String
nodeToYAML' indent node =
  case node of
    NString s -> replicate indent ' ' ++ yamlString s
    NNumber n -> replicate indent ' ' ++ show n
    NBool True -> replicate indent ' ' ++ "true"
    NBool False -> replicate indent ' ' ++ "false"
    NNull -> replicate indent ' ' ++ "null"
    NArray xs ->
      if null xs
        then replicate indent ' ' ++ "[]"
        else concatMap (formatArrayItem indent) xs
    NObject fields ->
      if null fields
        then replicate indent ' ' ++ "{}"
        else concatMap (formatKeyValue indent) fields

formatArrayItem :: Int -> Node -> String
formatArrayItem indent node =
  case node of
    NString s -> replicate indent ' ' ++ "- " ++ yamlString s ++ "\n"
    NNumber n -> replicate indent ' ' ++ "- " ++ show n ++ "\n"
    NBool True -> replicate indent ' ' ++ "- true\n"
    NBool False -> replicate indent ' ' ++ "- false\n"
    NNull -> replicate indent ' ' ++ "- null\n"
    NArray _ ->
      replicate indent ' ' ++ "-\n" ++ nodeToYAML' (indent + 2) node ++ "\n"
    NObject _ ->
      replicate indent ' ' ++ "-\n" ++ nodeToYAML' (indent + 2) node ++ "\n"

formatKeyValue :: Int -> (String, Node) -> String
formatKeyValue indent (key, value) =
  case value of
    NString s ->
      replicate indent ' ' ++ key ++ ": " ++ yamlString s ++ "\n"
    NNumber n ->
      replicate indent ' ' ++ key ++ ": " ++ show n ++ "\n"
    NBool True ->
      replicate indent ' ' ++ key ++ ": true\n"
    NBool False ->
      replicate indent ' ' ++ key ++ ": false\n"
    NNull ->
      replicate indent ' ' ++ key ++ ": null\n"
    NArray [] ->
      replicate indent ' ' ++ key ++ ": []\n"
    NArray _ ->
      replicate indent ' '
        ++ key
        ++ ":\n"
        ++ nodeToYAML' (indent + 2) value
        ++ "\n"
    NObject [] ->
      replicate indent ' ' ++ key ++ ": {}\n"
    NObject _ ->
      replicate indent ' '
        ++ key
        ++ ":\n"
        ++ nodeToYAML' (indent + 2) value
        ++ "\n"

yamlString :: String -> String
yamlString s =
  if needsQuoting s
    then show s
    else s

needsQuoting :: String -> Bool
needsQuoting s =
  null s
    || any (`elem` " \":-{}[],") s
    || s == "true"
    || s == "false"
    || s == "null"
