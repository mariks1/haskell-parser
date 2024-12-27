module Main where

import System.IO (hGetContents, stdin)
import Parser.XML (parseXML)
import Converter (nodeToJSON)
import Data.Either (either)

main :: IO ()
main = do
  -- Чтение XML-данных из стандартного ввода
  input <- hGetContents stdin

  -- Парсинг XML-строки в XML-структуру
  let parsedXML = parseJSON input

-- Преобразование Node в форматированный JSON
  let jsonOutput = nodeToJSON parsedXML

-- Вывод результата
  putStrLn jsonOutput
