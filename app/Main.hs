module Main (main) where

import Control.DeepSeq (deepseq)
import Converter (nodeToJSON, nodeToYAML, prettyNodeToXML)
import Parser.JSON (parseJSON)
import Parser.XML (parseXML)
import Parser.YAML (parseYAML)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO (hFlush, hGetContents, stdin, stdout)

main :: IO ()
main = do
  args <- getArgs

  (inputSource, inputFormat) <- case args of
    [] -> do
      putStrLn "Reading input from stdin. Enter input format (.xml, .json or .yaml):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile] | inputFile == "_" -> do
      putStrLn "Reading input from stdin. Enter input format (.xml, .json or .yaml):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile] -> pure (Right inputFile, takeExtension inputFile)
    [inputFile, _] | inputFile == "_" -> do
      putStrLn "Reading input from stdin. Enter input format (.xml, .json or .yaml):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile, _] -> pure (Right inputFile, takeExtension inputFile)
    _ -> error "Usage: lab4-exe [inputFile] [outputFile]"

  outputFormat <- case args of
    ["_", outputFile] -> pure (takeExtension outputFile)
    [] -> do
      putStrLn "Enter output format (.xml, .json or .yaml):"
      getLine
    [_, outputFile] -> pure (takeExtension outputFile)
    [_] -> do
      putStrLn "Enter output format (.xml, .json or .yaml):"
      getLine
    _ -> error "Invalid arguments. Usage: lab4-exe [inputFile] [outputFile]"

  -- Читаем входные данные
  input <- case inputSource of
    Left hIn -> do
      putStrLn "Reading input from stdin. Press Ctrl+D (or Ctrl+Z on Windows) to finish."
      hFlush stdout -- На всякий случай принудительно выведем подсказку
      contents <- hGetContents hIn

      -- Форсируем чтение всех данных
      contents `deepseq` pure contents
    Right inputFile -> do
      contents <- readFile inputFile
      contents `deepseq` pure contents

  let parsedData = case inputFormat of
        ".xml" -> parseXML input
        ".json" -> parseJSON input
        ".yaml" -> parseYAML input
        _ -> error "Unsupported input format. Use .xml, .json or .yaml"

  let output = case outputFormat of
        ".xml" -> prettyNodeToXML parsedData
        ".json" -> nodeToJSON parsedData
        ".yaml" -> nodeToYAML parsedData
        _ -> error "Unsupported output format. Use .xml or .json"

  case args of
    [] -> putStrLn output
    [_, outputFile] -> writeFile outputFile output
    [_] -> putStrLn output
    _ -> error "Invalid arguments. Usage: lab4-exe [inputFile] [outputFile]"
