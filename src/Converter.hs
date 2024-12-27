module Converter (nodeToJSON) where

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