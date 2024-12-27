module Main where

import System.IO (hGetContents, stdin)
-- import Converter.XML (parseXML)
import Parser.XML (parseXML, nodeToXML, xmlToNode)
import Converter (nodeToJSON)
import Parser.JSON (parseJSON)
import Data.Either (either)

main :: IO ()
main = do
  -- Чтение XML-данных из стандартного ввода
  input <- hGetContents stdin

  -- Парсинг XML-строки в XML-структуру
  let parsedXML = parseJSON input
  -- let parsedJSON = xmlToNode input
-- Преобразование Node в форматированный JSON
  -- let jsonOutput = nodeToJSON parsedJSON
  let jsonOutput = nodeToXML parsedXML
-- Вывод результата
  putStrLn jsonOutput
