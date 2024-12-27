module Parser.XML (parseXML) where

import Data.Char (isAlphaNum, isSpace)
import Types (Node (..))

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

spanWhile :: (Char -> Bool) -> String -> (String, String)
spanWhile _ [] = ([], [])
spanWhile predc (x : xs)
  | predc x = let (ys, zs) = spanWhile predc xs in (x : ys, zs)
  | otherwise = ([], x : xs)

parseXML :: String -> Node
parseXML str =
  let (node, _) = parseElement (skipSpaces str)
   in node

parseElement :: String -> (Node, String)
parseElement input =
  let (tagName, afterOpen) = parseOpenTag (skipSpaces input)

      (children, afterChildren) = parseContent tagName (skipSpaces afterOpen)

      afterClose = parseCloseTag tagName (skipSpaces afterChildren)

      node =
        case children of
          [] -> NObject [(tagName, NString "")]
          [c] -> NObject [(tagName, c)]
          cs -> NObject [(tagName, NArray cs)]
   in (node, afterClose)

parseOpenTag :: String -> (String, String)
parseOpenTag ('<' : xs) =
  let (tag, rest) = spanWhile (\c -> isAlphaNum c || c `elem` "-_:") xs
      rest' = skipSpaces rest
   in case rest' of
        ('>' : r2) -> (tag, r2)
        _ -> error $ "Expected '>' after tag name: <" ++ tag ++ "..."
parseOpenTag _ = error "Expected opening tag <..."

parseContent :: String -> String -> ([Node], String)
parseContent _ [] = ([], [])
parseContent _ input@('<' : '/' : _) =
  ([], input)
parseContent tagName input@('<' : _) =
  let (child, rest) = parseElement input
      (siblings, final) = parseContent tagName (skipSpaces rest)
   in (child : siblings, final)
parseContent tagName input =
  let (txt, rest) = spanWhile (/= '<') input
      textNode = NString (trim txt)
      (siblings, final) = parseContent tagName rest
   in (textNode : siblings, final)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseCloseTag :: String -> String -> String
parseCloseTag expectedTagName ('<' : '/' : xs) =
  let (tagName, rest) = spanWhile (\c -> isAlphaNum c || c `elem` "-_:") xs
      rest' = skipSpaces rest
   in if tagName /= expectedTagName
        then
          error $
            "Mismatched closing tag: expected </"
              ++ expectedTagName
              ++ ">, but got </"
              ++ tagName
              ++ ">"
        else case rest' of
          ('>' : r2) -> r2
          _ -> error $ "Expected '>' after </" ++ tagName ++ ">"
parseCloseTag expectedTagName _ =
  error $ "Expected closing tag </" ++ expectedTagName ++ ">"
