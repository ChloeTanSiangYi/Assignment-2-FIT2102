module Assignment (markdownParser, convertADTHTML) where

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser          (char, string, isNot, noneof, inlineSpace, satisfy, charTok, space, oneof, spaces, is, digit, stringTok)
import           Control.Applicative (Alternative (..), some, many)
import           Control.Monad (replicateM)

data ADT = Empty |
            StringADT String |
            Italic ADT |
            Bold ADT |
            Strikethrough ADT |
            Link ADT ADT |
            InlineCode ADT |
            Footnote ADT |
            Image ADT ADT ADT |
            URLCaption ADT|
            FootnoteReference ADT ADT |
            Freetext [ADT] |
            Heading Int ADT |
            BlockQuote [ADT] |
            CodeBlock ADT ADT |
            OrderList [ADT] |
            SubOrderedList [ADT] |
            UnorderedList [ADT] |
            SubUnorderedList [ADT] |
            Table [ADT] [ADT]|
            HeadRow [ADT] |
            DataCell [ADT] |
            Modifier ADT |
            RawHTML String
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

markdownParser :: Parser ADT
markdownParser = parserHTML <|> baseModifiers <|> parserHeader <|> parserBlockQuote <|> parserCodeBlock <|> parserOrderedList <|> parserUnorderedList <|> parserTable <|> parserFreetext <|> parserHorizontalRule

textCharParser :: String -> Parser ADT
textCharParser stopString = do
  content <- some (noneof stopString)
  return $ StringADT content

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (StringADT s) = s
convertADTHTML (Italic x) = "<em>" ++ convertADTHTML x ++ "</em>"
convertADTHTML (Bold x) = "<strong>" ++ convertADTHTML x ++ "</strong>"
convertADTHTML (Strikethrough x) = "<del>" ++ convertADTHTML x ++ "</del>"
convertADTHTML (Link x y) = "<a href=\"" ++ convertADTHTML y ++ "\">" ++ convertADTHTML x ++ "</a>"
convertADTHTML (InlineCode x) = "<code>" ++ convertADTHTML x ++ "</code>"
convertADTHTML (Footnote x) =
  "<sup><a id=\"" ++ convertADTHTML x ++ "\" href=\"#ref" ++ convertADTHTML x ++ "\">" 
  ++ convertADTHTML x ++ "</a></sup>"
convertADTHTML (Image alt url caption) =
  "<img src=\"" ++ convertADTHTML url ++ "\" alt=\"" ++ convertADTHTML alt ++ 
  "\" title=\"" ++ convertADTHTML caption ++ "\" />"
convertADTHTML (FootnoteReference id ref) =
  "<p id=\"ref" ++ convertADTHTML id ++ "\">" ++ convertADTHTML ref ++ "</p>"
convertADTHTML (Freetext xs) = concatMap convertADTHTML xs -- Handles multiple elements
convertADTHTML (Heading n x) =
  "<h" ++ show n ++ ">" ++ convertADTHTML x ++ "</h" ++ show n ++ ">"
convertADTHTML (BlockQuote xs) =
  "<blockquote>" ++ concatMap (\line -> "<p>" ++ convertADTHTML line ++ "</p>\n") xs ++ "</blockquote>"
convertADTHTML (CodeBlock lang x) =
  "<pre><code class=\"language-" ++ convertADTHTML lang ++ "\">" ++ convertADTHTML x ++ "</code></pre>"
convertADTHTML (OrderList xs) =
  "<ol>\n" ++ concatMap convertItem xs ++ "</ol>\n"
  where
    convertItem (SubOrderedList subitems) =
      "<ol>\n" ++ concatMap (\subitem -> "<li>" ++ convertADTHTML subitem ++ "</li>") subitems ++ "</ol>\n</li>"
    convertItem item = "<li>" ++ convertADTHTML item ++ "</li>\n"
convertADTHTML (SubOrderedList xs) =
  concatMap (\y -> "<li>" ++ convertADTHTML y ++ "</li>\n") xs
convertADTHTML (Table headRow dataCells) =
  "<table>\n" ++ concatMap convertADTHTML headRow ++ concatMap convertADTHTML dataCells ++ "</table>"
convertADTHTML (HeadRow xs) =
  "<tr>\n" ++ concatMap (\cell -> "    <th>" ++ convertADTHTML cell ++ "</th>\n") xs ++ "</tr>"
convertADTHTML (DataCell xs) =
  "<tr>\n" ++ concatMap (\cell -> "    <td>" ++ convertADTHTML cell ++ "</td>\n") xs ++ "</tr>"
convertADTHTML (Modifier x) = convertADTHTML x
convertADTHTML (RawHTML x) = x
convertADTHTML _ = "test" -- Default case for unmatched patterns

otherlines :: ADT -> [ADT]
otherlines (Freetext x) = x
otherlines _ = []
-- Text modifiers

parserModifier :: Parser ADT
parserModifier = Modifier <$> (spaces *> (parserItalic <|> parserBold <|> parserStrike <|> parserLink <|> parserInlineCode <|> parserFootnote <|> parserImage <|> parserFootnoteReference <|> parserFreetext) <* spaces)

parserItalic :: Parser ADT
parserItalic = do
  _ <- string "_"
  _ <- inlineSpace
  x <- textCharParser "_"
  _ <- inlineSpace
  _ <- string "_"
  return $ Italic x

parserBold :: Parser ADT
parserBold = do
  _ <- string "**"
  _ <- inlineSpace
  x <- textCharParser "**"
  _ <- inlineSpace
  _ <- string "**"
  return $ Bold x

parserStrike :: Parser ADT
parserStrike = do
  _ <- string "~~"
  _ <- inlineSpace
  x <- textCharParser "~~"
  _ <- inlineSpace
  _ <- string "~~"
  return $ Strikethrough x

parserLink :: Parser ADT
parserLink = do
  _ <- charTok '['
  _ <- inlineSpace
  linkText <- textCharParser "]"
  _ <- inlineSpace
  _ <- string "]("
  linkURL <- textCharParser ")"
  _ <- inlineSpace
  _ <- charTok ')'
  return $ Link linkText linkURL

parserNestedLink :: Parser ADT
parserNestedLink = do
  _ <- charTok '['
  linkText <- parsePlainText
  _ <- charTok ']'
  _ <- charTok '('
  linkURL <- textCharParser ")"
  return $ Link linkText linkURL

parserInlineCode :: Parser ADT
parserInlineCode = do
  _ <- charTok '`'
  _ <- inlineSpace
  x <- textCharParser "`"
  _ <- inlineSpace
  _ <- charTok '`'
  return $ InlineCode x

parserFootnote :: Parser ADT
parserFootnote = do
  _ <- string "[^"
  number <- StringADT <$> some digit
  _ <- string "]"
  return $ Footnote number

parserImage :: Parser ADT
parserImage = do
  _ <- string "!["
  alt <- textCharParser "]"
  _ <- inlineSpace
  _ <- string "]("
  url <- textCharParser " "
  _ <- inlineSpace
  _ <- string "\""
  caption <- textCharParser "\""
  _ <- string "\""
  _ <- string ")"
  return $ Image alt url caption


parserFootnoteReference :: Parser ADT
parserFootnoteReference = do
  footnote <- parserFootnote
  _ <- charTok ':'
  _ <- inlineSpace
  reference <- textCharParser "\n"
  _ <- charTok '\n'
  return $ FootnoteReference footnote reference

baseModifiers :: Parser ADT
baseModifiers = parserItalic <|> parserBold <|> parserStrike <|> parserLink  <|> parserNestedLink
               <|> parserInlineCode <|> parserFootnote <|> parserImage 
               <|> parserFootnoteReference

-- Helper parser for plain text
parsePlainText :: Parser ADT
parsePlainText = do
  content <- some (noneof ['_', '*', '~', '[', '`', '!', '\n'])
  return $ StringADT content

parserFreetext :: Parser ADT
parserFreetext = do
  elements <- many (baseModifiers <|> parsePlainText) -- This allows baseModifiers to parse inside Freetext, enabling nesting of bold and italic text
  return $ Freetext elements

parserHeader :: Parser ADT
parserHeader = parserHeading <|> parserAlternativeHeading1 <|> parserAlternativeHeading2

parserHeading :: Parser ADT
parserHeading = do
  hashes <- some (is '#')
  _ <- is ' '
  _ <- spaces
  content <- baseModifiers <|> textCharParser "\n"
  _ <- charTok '\n'
  return $ Heading (length hashes) content

parserAlternativeHeading1 :: Parser ADT
parserAlternativeHeading1 = do
  content <- textCharParser "\n" <|> parserModifier
  _ <- charTok '\n'
  _ <- some (charTok '=')
  return $ Heading 1 content

parserAlternativeHeading2 :: Parser ADT
parserAlternativeHeading2 = do
  content <- textCharParser "\n" <|> parserModifier
  _ <- charTok '\n'
  _ <- some (charTok '-')
  return $ Heading 2 content

parserBlockQuote :: Parser ADT
parserBlockQuote = do
  _ <- string ">"
  _ <- inlineSpace
  firstLine <- baseModifiers <|> textCharParser "\n"
  restLines <- many (nestedBlockQuote <|> (charTok '\n' *> string "> " *> (baseModifiers <|> textCharParser "\n")))
  _ <- charTok '\n'
  return $ BlockQuote (firstLine : restLines)

nestedBlockQuote :: Parser ADT
nestedBlockQuote = do
  _ <- string ">>"
  _ <- inlineSpace
  content <- parserBlockQuote  -- Recursively parse nested blockquotes
  return content

parserCodeBlock :: Parser ADT
parserCodeBlock = do
  _ <- string "```"
  _ <- inlineSpace
  language <- textCharParser "\n"
  _ <- charTok '\n'
  content <- textCharParser "```"
  _ <- string "```"
  return $ CodeBlock language content

parserOrdrListItem :: Parser ADT
parserOrdrListItem = do
  _ <- some digit
  _ <- is '.'
  _ <- is ' '
  text <- baseModifiers <|> textCharParser "\n"
  _ <- is '\n'
  return text

parserUnordListItem :: Parser ADT
parserUnordListItem = do
    _ <- oneof ['*', '-', '+']  -- Support for bullet symbols
    _ <- is ' '
    text <- baseModifiers <|> textCharParser "\n"
    _ <- is '\n'
    return text

parserOrderedList :: Parser ADT
parserOrderedList = do
  firstItem <- parserOrdrListItem
  restItems <- many (parserSubList <|> parserOrdrListItem)
  return $ OrderList (firstItem : restItems)

parserUnorderedList :: Parser ADT
parserUnorderedList = do
    firstItem <- parserUnordListItem
    restItems <- many (parserSubUnorderedList <|> parserUnordListItem)
    return $ UnorderedList (firstItem : restItems)

parserSubList :: Parser ADT
parserSubList = do
  _ <- string "    "
  firstItem <- parserOrdrListItem
  restItems <- many (string "    " *> parserOrdrListItem)
  return $ SubOrderedList (firstItem : restItems)

parserSubUnorderedList :: Parser ADT
parserSubUnorderedList = do
    _ <- string "    "  -- Assuming 4 spaces for indentation
    firstItem <- parserUnordListItem
    restItems <- many (string "    " *> parserUnordListItem)
    return $ SubUnorderedList (firstItem : restItems)

parserTable :: Parser ADT
parserTable = do
  headRow <- parserHeadRow
  _ <- parserTableSeparator
  dataCells <- some parserDataCell
  return $ Table [headRow] dataCells

parserHeadRow :: Parser ADT
parserHeadRow = do
  _ <- charTok '|'
  cells <- some ( baseModifiers  <|> textCharParser "|" <* charTok '|')
  return $ HeadRow cells

parserDataCell :: Parser ADT
parserDataCell = do
  _ <- charTok '|'
  cells <- some (baseModifiers <|> textCharParser "|" <* charTok '|')
  return $ DataCell cells

parserTableSeparator :: Parser ADT
parserTableSeparator = do
  _ <- charTok '|'
  _ <- some (stringTok "---" <* many (charTok '-') <* charTok '|')
  return (StringADT "Separator")

parserHorizontalRule :: Parser ADT
parserHorizontalRule = do
  _ <- some (oneof "-*")
  _ <- spaces
  _ <- charTok '\n'
  return $ StringADT "<hr />"

parserHTML :: Parser ADT
parserHTML = do
  _ <- string "<div>" <|> string "<p>" -- Start of HTML element
  content <- many (noneof "</>") -- Capture content until the closing tag
  _ <- string "</div>" <|> string "</p>" -- End of HTML element
  return $ RawHTML content