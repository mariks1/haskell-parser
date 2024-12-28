module Parser.XML (parseXML) where

import Data.Char (isAlphaNum, isSpace)
import Types (Node (..))

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

spanWhile :: (Char -> Bool) -> String -> (String, String)
spanWhile _ [] = ([], [])
spanWhile p (x:xs)
  | p x =
      let (ys,zs) = spanWhile p xs
      in (x:ys, zs)
  | otherwise = ([], x:xs)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Основная точка входа
parseXML :: String -> Node
parseXML str =
  let (node, rest) = parseElement (skipSpaces str)
      leftover = skipSpaces rest
  in
    if not (null leftover)
      then error $ "Extra data at end: " ++ take 50 leftover
      else normalize node

-- Парсим один элемент <tag> ... </tag>
parseElement :: String -> (Node, String)
parseElement input =
  let (tagName, afterOpen) = parseOpenTag (skipSpaces input)
  in case tagName of
       "array" ->
         let (children, afterChildren) = parseContent "array" (skipSpaces afterOpen)
             afterClose = parseCloseTag "array" (skipSpaces afterChildren)
         in (NObject [("array", NArray children)], afterClose)

       "item" ->
         let (children, afterChildren) = parseContent "item" (skipSpaces afterOpen)
             afterClose = parseCloseTag "item" (skipSpaces afterChildren)
         in (NObject [("item", NArray children)], afterClose)

       _ ->
         let (children, afterChildren) = parseContent tagName (skipSpaces afterOpen)
             afterClose = parseCloseTag tagName (skipSpaces afterChildren)
         in
           case children of
             []  -> (NObject [(tagName, NString "")], afterClose)
             [c] -> (NObject [(tagName, c)],         afterClose)
             cs  -> (NObject [(tagName, NArray cs)], afterClose)

parseOpenTag :: String -> (String, String)
parseOpenTag ('<' : xs) =
  let (tag, rest) = spanWhile (\c -> isAlphaNum c || c `elem` "-_:") xs
      rest' = skipSpaces rest
  in case rest' of
       ('>' : r2) -> (tag, r2)
       _ -> error $ "Expected '>' after tag name <" ++ tag ++ "..."
parseOpenTag _ = error "Expected <tag>"

-- Парсим содержимое (до </tagName>)
parseContent :: String -> String -> ([Node], String)
parseContent _ [] = ([], [])
parseContent _ input@('<' : '/' : _) =
  ([], input)  
parseContent parentTag input@('<' : _) =
  let (childNode, rest) = parseElement input
      (siblings, final) = parseContent parentTag (skipSpaces rest)
  in (childNode : siblings, final)
parseContent parentTag input =
  let (txt, rest) = spanWhile (/= '<') input
      txt' = trim txt
      nodeIfNotEmpty =
        if null txt'
          then []
          else [NString txt']
      (siblings, final) = parseContent parentTag rest
  in (nodeIfNotEmpty ++ siblings, final)

parseCloseTag :: String -> String -> String
parseCloseTag expectedTagName ('<' : '/' : xs) =
  let (tagName, rest) = spanWhile (\c -> isAlphaNum c || c `elem` "-_:") xs
      rest' = skipSpaces rest
  in if tagName /= expectedTagName
       then error $ "Mismatched closing tag: expected </"
                    ++ expectedTagName ++ ">, got </" ++ tagName ++ ">"
       else case rest' of
         ('>' : r2) -> r2
         _ -> error $ "Expected '>' after </" ++ tagName ++ ">"
parseCloseTag expectedTagName stuff =
  error $ "Expected </" ++ expectedTagName ++ ">, got: " ++ take 50 stuff

normalize :: Node -> Node
normalize (NObject [(k, v)])
  | k == "array" =
      case v of
        NArray items -> NArray (map normalize items)
        _            -> NArray []

  | k == "item" =
      case v of
        NArray [NString s] ->
          normalize (NString s)

        NArray [single] ->
          normalize single
        NArray fields ->
          let pairs = concatMap collectFields fields
          in NObject pairs

        _ -> NNull

  | k == "root" =
      case v of
        NArray items ->
          let normalizedItems = map normalize items
              mergedPairs = concatMap (\node -> case node of
                                         NObject fs -> fs
                                         _          -> []) normalizedItems
          in NObject mergedPairs

        _ -> normalize v

  | otherwise =
      NObject [(k, normalize v)]

normalize (NObject fields) =
  NObject $ map (\(k, v) -> (k, normalize v)) fields

normalize (NArray xs) =
  NArray (map normalize xs)

normalize (NString s) =
  case parseNumericOrBool (trim s) of
    Just val -> val
    Nothing  -> NString (trim s)

normalize x = x

collectFields :: Node -> [(String, Node)]
collectFields (NObject [(key, val)]) = [(key, normalize val)]
collectFields (NObject fields)       = map (\(k,v) -> (k, normalize v)) fields
collectFields _                      = []

parseNumericOrBool :: String -> Maybe Node
parseNumericOrBool "true"  = Just (NBool True)
parseNumericOrBool "false" = Just (NBool False)
parseNumericOrBool s =
  case reads s :: [(Double, String)] of
    [(num, "")] -> Just (NNumber num)
    _           -> Nothing
