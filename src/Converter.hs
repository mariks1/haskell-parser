module Converter (nodeToJSON, prettyNodeToXML) where

import Types

-- Преобразование Node в JSON-строку (с отступами)
nodeToJSON :: Node -> String
nodeToJSON node = formatNode 0 node

-- Основная функция форматирования
formatNode :: Int -> Node -> String
formatNode indent (NString s) = show s -- Экранирует строку автоматически
formatNode indent (NNumber n) = show n
formatNode indent (NBool True) = "true"
formatNode indent (NBool False) = "false"
formatNode indent NNull = "null"
formatNode indent (NArray nodes) =
  "[\n" ++ formatArray (indent + 2) nodes ++ "\n" ++ replicate indent ' ' ++ "]"
formatNode indent (NObject kvs) =
  "{\n" ++ formatObject (indent + 2) kvs ++ "\n" ++ replicate indent ' ' ++ "}"

-- Преобразование массива Node в строку JSON (с отступами)
formatArray :: Int -> [Node] -> String
formatArray indent [] = replicate indent ' ' ++ "" -- Пустой массив
formatArray indent nodes =
  concatMap (\node -> replicate indent ' ' ++ formatNode indent node ++ ",\n") (init nodes)
    ++ replicate indent ' ' ++ formatNode indent (last nodes)

-- Преобразование объекта (списка ключ-значение) в строку JSON (с отступами)
formatObject :: Int -> [(String, Node)] -> String
formatObject indent [] = replicate indent ' ' ++ "" -- Пустой объект
formatObject indent kvs =
  concatMap
    (\(key, value) ->
       replicate indent ' ' ++ "\"" ++ key ++ "\": " ++ formatNode indent value ++ ",\n")
    (init kvs)
    ++ replicate indent ' ' ++
       "\"" ++ fst (last kvs) ++ "\": " ++ formatNode indent (snd (last kvs))

-- Функция для преобразования Node в XML
-- Форматированный вывод XML с отступами
prettyNodeToXML :: Node -> String
prettyNodeToXML node = prettyNodeToXML' node 0

-- Вспомогательная функция с уровнем отступа
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
    concatMap (\node -> prettyNodeToXML' node (indent + 2)) nodes
prettyNodeToXML' (NObject fields) indent =
    concatMap (\(key, value) ->
        replicate indent ' ' ++ "<" ++ escape key ++ ">\n" ++
        prettyNodeToXML' value (indent + 2) ++ "\n" ++
        replicate indent ' ' ++ "</" ++ escape key ++ ">\n"
    ) fields

-- Экранирование специальных символов для XML
escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c    = [c]
