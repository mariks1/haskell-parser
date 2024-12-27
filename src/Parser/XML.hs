module Parser.XML where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile, writeFile)
import Data.Char (isSpace)
import Control.Applicative ((<|>))
import Control.Monad (void, guard)
import Data.List (isPrefixOf)
import Types
-- Конвертация XML в Node
xmlToNode :: XML -> Node
xmlToNode (XText text) = NString text
xmlToNode (XElem name attrs children) =
    NObject $
        [("tag", NString name)] ++
        [("attributes", attrsToNode attrs)] ++
        [("children", childrenToNode children)]

attrsToNode :: [(String, String)] -> Node
attrsToNode attrs = NObject $ map (\(k,v) -> (k, NString v)) attrs

childrenToNode :: [XML] -> Node
childrenToNode children = NArray (map xmlToNode children)

-- Метод для преобразования Node обратно в XML
nodeToXML :: Node -> String
nodeToXML (NObject fields) =
    case lookup "tag" fields of
        Just (NString tagName) ->
            let attributes = case lookup "attributes" fields of
                                Just (NObject attrs) -> attrs
                                _ -> []
                children = case lookup "children" fields of
                                Just (NArray childs) -> childs
                                _ -> []
                attrsStr = concatMap (\(k, NString v) -> " " ++ k ++ "=\"" ++ escape v ++ "\"") attributes
                childrenStr = concatMap nodeToXML children
            in "<" ++ tagName ++ attrsStr ++ ">" ++ childrenStr ++ "</" ++ tagName ++ ">"
        _ -> ""
nodeToXML (NString text) = escape text
nodeToXML _ = ""

-- Функция для экранирования специальных символов в XML
escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c    = [c]

-- Простая функция для преобразования XML в строку с отступами (для читаемости)
prettyPrintXML :: String -> String
prettyPrintXML xmlStr = unlines $ prettyLines 0 (nodeToXML $ xmlToNode parsedXML)
  where
    parsedXML = case parseXML xmlStr of
                    Right xml -> xml
                    Left _    -> XText xmlStr -- В случае ошибки просто вернуть текст

prettyLines :: Int -> String -> [String]
prettyLines indent xml = case parseXML xml of
    Right (XElem tag attrs children) ->
        let indentation = replicate (indent * 2) ' '
            attrsStr = concatMap (\(k,v) -> " " ++ k ++ "=\"" ++ v ++ "\"") attrs
            openTag = indentation ++ "<" ++ tag ++ attrsStr ++ ">"
            childLines = concatMap (prettyLines (indent + 1) . nodeToXML . xmlToNode) children
            closeTag = indentation ++ "</" ++ tag ++ ">"
        in [openTag] ++ childLines ++ [closeTag]
    Right (XText text) ->
        [replicate (indent * 2) ' ' ++ escape text]
    Left _ ->
        [replicate (indent * 2) ' ' ++ xml]

-- Простой XML парсер
-- Примечание: Этот парсер очень упрощён и не обрабатывает все возможные случаи XML.
parseXML :: String -> Either String XML
parseXML input = case parseElement (trim input) of
    Just (elem, rest) | all isSpace rest -> Right elem
    _ -> Left "Не удалось полностью разобрать XML."

parseElement :: String -> Maybe (XML, String)
parseElement s =
    parseTag s <|> parseText s

parseTag :: String -> Maybe (XML, String)
parseTag s = do
    let s' = dropWhile isSpace s
    guard (not (null s') && head s' == '<')
    let s'' = tail s'
    -- Проверка на закрывающий тег
    guard (not (take 2 s'' == "//"))
    -- Парсинг имени тега
    let (tagName, rest1) = span (\c -> not (isSpace c) && c /= '>' && c /= '/') s''
    -- Парсинг атрибутов
    let (attrs, rest2) = parseAttributes rest1
    -- Проверка на самозакрывающийся тег
    if take 2 rest2 == "/>"
        then Just (XElem tagName attrs [], drop 2 rest2)
    else if not (null rest2) && head rest2 == '>'
        then do
            -- Парсинг дочерних элементов
            let restAfterTag = tail rest2
            (children, rest3) <- parseChildren restAfterTag tagName
            return (XElem tagName attrs children, rest3)
    else
        Nothing

parseAttributes :: String -> ([(String, String)], String)
parseAttributes s =
    let s' = dropWhile isSpace s
    in if null s' || head s' `elem` ['>', '/']
        then ([], s')
        else
            let (attrName, rest) = span (\c -> not (isSpace c) && c /= '=') s'
                rest' = dropWhile (/= '"') rest
            in if null rest'
                then ([], s')
                else
                    let value = takeWhile (/= '"') (tail rest')
                        rest'' = drop 1 (dropWhile (/= '"') rest)
                        (attrs, finalRest) = parseAttributes rest''
                    in ((attrName, value) : attrs, finalRest)

parseChildren :: String -> String -> Maybe ([XML], String)
parseChildren s parentTag = go [] s
  where
    closingTag = "</" ++ parentTag ++ ">"
    go acc str
        | take (length closingTag) str == closingTag = Just (reverse acc, drop (length closingTag) str)
        | otherwise =
            case parseElement str of
                Just (child, rest) -> go (child : acc) rest
                Nothing ->
                    -- Если не удалось разобрать элемент, попытаться разобрать текст
                    case parseText str of
                        Just (txt, restTxt) -> go (txt : acc) restTxt
                        Nothing -> Nothing

parseText :: String -> Maybe (XML, String)
parseText s =
    let (txt, rest) = span (/= '<') s
    in if null txt
        then Nothing
        else Just (XText (trim txt), rest)

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = prefix == take (length prefix) str

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace