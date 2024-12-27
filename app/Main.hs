module Main where

import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO (hGetContents, hFlush, hPutStrLn, stdin, stdout)
import Control.DeepSeq (deepseq)
import Converter (nodeToJSON, prettyNodeToXML)
import Parser.JSON (parseJSON)
import Parser.XML (parseXML)

main :: IO ()
main = do
  args <- getArgs

  (inputSource, inputFormat) <- case args of
    [] -> do
      putStrLn "Reading input from stdin. Enter input format (.xml or .json):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile] | inputFile == "_" -> do
      putStrLn "Reading input from stdin. Enter input format (.xml or .json):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile] -> pure (Right inputFile, takeExtension inputFile)
    [inputFile, _] | inputFile == "_" -> do
      putStrLn "Reading input from stdin. Enter input format (.xml or .json):"
      fmt <- getLine
      pure (Left stdin, fmt)
    [inputFile, _] -> pure (Right inputFile, takeExtension inputFile)
    _ -> error "Usage: lab4-exe [inputFile] [outputFile]"

  outputFormat <- case args of
    [] -> do
      putStrLn "Enter output format (.xml or .json):"
      getLine
    [_, outputFile] -> pure (takeExtension outputFile)
    ["_", outputFile] -> pure (takeExtension outputFile)
    [_] -> do
      putStrLn "Enter output format (.xml or .json):"
      getLine

  -- Читаем входные данные
  input <- case inputSource of
    Left hIn -> do
      putStrLn "Reading input from stdin. Press Ctrl+D (or Ctrl+Z on Windows) to finish."
      hFlush stdout  -- На всякий случай принудительно выведем подсказку
      contents <- hGetContents hIn

      -- Форсируем чтение всех данных
      contents `deepseq` pure contents

    Right inputFile -> do
      contents <- readFile inputFile
      contents `deepseq` pure contents

  let parsedData = case inputFormat of
        ".xml"  -> parseXML input
        ".json" -> parseJSON input
        _       -> error "Unsupported input format. Use .xml or .json"

  let output = case outputFormat of
        ".xml"  -> prettyNodeToXML parsedData
        ".json" -> nodeToJSON parsedData
        _       -> error "Unsupported output format. Use .xml or .json"

  case args of
    [_, outputFile] -> writeFile outputFile output
    ["_", outputFile] -> writeFile outputFile output
    _ -> putStrLn output
