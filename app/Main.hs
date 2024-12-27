module Main where

import System.IO (hGetContents, stdin)
import Parser.XML (parseXML)
import Converter (nodeToJSON)
import Data.Either (either)

main :: IO ()
main = do

  input <- hGetContents stdin

  let parsedXML = parseJSON input

  let jsonOutput = nodeToJSON parsedXML

  putStrLn jsonOutput
