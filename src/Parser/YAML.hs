module Parser.YAML (parseYAML) where

import Data.Char (isSpace)
import Types (Node (..))

-- Проверяем, является ли строка булевым литералом
parseBool :: String -> Maybe Bool
parseBool s
  | s == "true" = Just True
  | s == "false" = Just False
  | otherwise = Nothing

-- Проверяем, является ли строка null
parseNull :: String -> Bool
parseNull s = s == "null"

-- Проверяем, является ли строка числом (упрощённо)
parseNumber :: String -> Maybe Double
parseNumber s =
  case reads s :: [(Double, String)] of
    [(num, "")] -> Just num
    _ -> Nothing

-- Определяем, сколько пробелов в начале строки (уровень отступа)
countIndent :: String -> Int
countIndent = length . takeWhile (== ' ')

-- Разбиваем весь текст на (indent, строкаБезНачальныхПробелов)
splitLines :: String -> [(Int, String)]
splitLines input =
  [ (countIndent line, dropWhile isSpace line)
    | rawLine <- lines input,
      let line = rstrip rawLine,
      not (null line)
  ]
  where
    rstrip = reverse . dropWhile isSpace . reverse

-- | Парсит YAML-текст и возвращает корневой `Node`
parseYAML :: String -> Node
parseYAML text =
  let ls = splitLines text
   in case parseBlock 0 ls of
        (node, []) -> node
        (_, leftover) ->
          error $ "Необработанные строки после парсинга: " ++ show leftover

parseBlock :: Int -> [(Int, String)] -> (Node, [(Int, String)])
parseBlock _ [] = (NNull, [])
parseBlock indent allLines@((i, l) : _)
  | i < indent =
      (NNull, allLines)
  | isListItem l =
      parseSequence indent allLines
  | isKeyValue l =
      parseMapping indent allLines
  | otherwise =
      let (scalarNode, rest) = parseScalar indent allLines
       in (scalarNode, rest)

isListItem :: String -> Bool
isListItem l = case dropWhile isSpace l of
  ('-' : ' ' : _) -> True
  _ -> False

parseSequence :: Int -> [(Int, String)] -> (Node, [(Int, String)])
parseSequence indent = go []
  where
    go acc [] = (NArray (reverse acc), [])
    go acc ((i, l) : xs)
      | i == indent,
        Just itemStr <- stripDash l =
          let (node, rest) = parseValueOrNested (indent + 2) (i, itemStr) xs
           in go (node : acc) rest
      | i == indent,
        isListItem l =
          error $ "Некорректный элемент списка: " ++ l
      | i < indent =
          (NArray (reverse acc), (i, l) : xs)
      | otherwise =
          (NArray (reverse acc), (i, l) : xs)

    stripDash :: String -> Maybe String
    stripDash s =
      let s' = dropWhile isSpace s
       in case s' of
            ('-' : ' ' : rest) -> Just rest
            _ -> Nothing

isKeyValue :: String -> Bool
isKeyValue l =
  case break (== ':') l of
    (_, "") -> False
    (_, _ : _) -> True

parseMapping :: Int -> [(Int, String)] -> (Node, [(Int, String)])
parseMapping indent = go []
  where
    go acc [] = (NObject (reverse acc), [])
    go acc ((i, l) : xs)
      | i < indent =
          (NObject (reverse acc), (i, l) : xs)
      | isKeyValue l =
          let (key, restStr) = break (== ':') l
              valStr = drop 1 restStr
              key' = trim key

              (nodeValue, restLines) = parseValueOrNested (indent + 2) (i, valStr) xs
              pair = (key', nodeValue)
           in go (pair : acc) restLines
      | otherwise =
          (NObject (reverse acc), (i, l) : xs)

parseScalar :: Int -> [(Int, String)] -> (Node, [(Int, String)])
parseScalar _ [] = (NNull, [])
parseScalar _ ((_, l) : xs) =
  let trimmed = trim l
   in (stringToNode trimmed, xs)

stringToNode :: String -> Node
stringToNode s
  | Just b <- parseBool s = NBool b
  | parseNull s = NNull
  | Just n <- parseNumber s = NNumber n
  | otherwise = NString s

parseValueOrNested ::
  Int ->
  (Int, String) ->
  [(Int, String)] ->
  (Node, [(Int, String)])
parseValueOrNested nestedIndent (_, valStr) xs =
  let valStr' = trim valStr
   in if null valStr'
        then parseBlock nestedIndent xs
        else (stringToNode valStr', xs)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
