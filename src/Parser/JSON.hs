module Parser.JSON (parseJSON) where

import Data.Char (isDigit, isSpace)
import Data.List (uncons)
import Types

-- Удалить пробелы
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- Парсинг строки (например, `"hello"`)
parseString :: String -> (Node, String)
parseString ('"' : xs) =
  case span (/= '"') xs of
    (str, '"' : rest) -> (NString str, rest)
    _ -> error "Unterminated string"
parseString _ = error "Expected string"

-- Парсинг числа (например, `123` или `-45.67`)
parseNumber :: String -> (Node, String)
parseNumber xs = (NNumber (read num), rest)
  where
    (num, rest) = span (\c -> isDigit c || c == '.' || c == '-') xs

-- Парсинг `true`, `false`, `null`
parseLiteral :: String -> (Node, String)
parseLiteral xs
  | take 4 xs == "true" = (NBool True, drop 4 xs)
  | take 5 xs == "false" = (NBool False, drop 5 xs)
  | take 4 xs == "null" = (NNull, drop 4 xs)
  | otherwise = error "Expected literal (true, false, null)"

-- Парсинг массива (например, `[1, 2, "hello"]`)
parseArray :: String -> (Node, String)
parseArray ('[' : xs) = (NArray values, skipSpaces rest)
  where
    (values, rest) = parseElements (skipSpaces xs)
    parseElements (']' : ys) = ([], ys)
    parseElements ys =
      let (v, rest') = parseValue ys
          rest'' = skipSpaces rest'
       in case rest'' of
            (',' : rest''') ->
              let (vs, finalRest) = parseElements (skipSpaces rest''') in (v : vs, finalRest)
            (']' : rest''') ->
              ([v], rest''')
            _ -> error "Expected ',' or ']' in array"
parseArray _ = error "Expected array"

-- Парсинг объекта (например, `{"key": "value"}`)
parseObject :: String -> (Node, String)
parseObject ('{' : xs) = (NObject pairs, skipSpaces rest)
  where
    (pairs, rest) = parsePairs (skipSpaces xs)

    -- Парсинг пар ключ-значение
    parsePairs :: String -> ([(String, Node)], String)
    parsePairs ('}' : ys) = ([], ys)
    parsePairs ys =
      let (key, rest1) = parseString ys
          rest2 = skipSpaces rest1
       in case rest2 of
            (':' : rest3) ->
              let (value, rest4) = parseValue (skipSpaces rest3)
                  (keyValuePairs, finalRest) =
                    case skipSpaces rest4 of
                      (',' : rest5) -> parsePairs (skipSpaces rest5)
                      ('}' : rest5) -> ([], rest5)
                      _ -> error "Expected ',' or '}' in object"
               in ((keyString key, value) : keyValuePairs, finalRest)
            _ -> error "Expected ':' after object key"

    keyString (NString s) = s
    keyString _ = error "Expected string as object key"
parseObject _ = error "Expected object"

-- Парсинг значения
parseValue :: String -> (Node, String)
parseValue xs =
  case skipSpaces xs of
    ('"' : _) -> parseString xs
    ('[' : _) -> parseArray xs
    ('{' : _) -> parseObject xs
    xs' ->
      case uncons xs' of
        Just (c, _) | isDigit c || c == '-' -> parseNumber xs'
        _
          | take 4 xs' == "true" || take 5 xs' == "false" || take 4 xs' == "null" ->
              parseLiteral xs'
        _ -> error "Unknown value"

-- Основной парсер JSON
parseJSON :: String -> Node
parseJSON = fst . parseValue . skipSpaces
