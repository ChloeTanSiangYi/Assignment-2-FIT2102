module Assignment (markdownParser, convertADTHTML) 
where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        
import           Parser 
import          Control.Applicative (Alternative (..))          

data ADT =
  Empty |
  ParseErrorADT ParseError |
  URL ADT ADT |
  Image ADT ADT |
  StringADT String |
  QuoteADT ADT |
  SquareBrackets ADT |
  RoundBrackets ADT |
  Italic ADT |
  Bold ADT |
  Strikethrough ADT |
  Link ADT ADT |
  InlineCode ADT |
  Footnote ADT |
  Modifier ADT |
  FootnoteReference ADT ADT |
  FreeText [ADT] |
  Header Int ADT |
  BlockQuote ADT |
  CodeBlock (Maybe String) String |
  List [ADT] |
  Table [[ADT]]

  deriving (Show, Eq)

markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
-- text modifiers
convertADTHTML (Italic content) = "<em>" ++ convertADTHTML content ++ "</em>"
convertADTHTML (Bold content) = "<strong>" ++ convertADTHTML content ++ "</strong>"
convertADTHTML (Strikethrough content) = "<del>" ++ convertADTHTML content ++ "/<del>"
convertADTHTML (Link text url) = "<a href=\"" ++ extractString url ++ "\">" ++ convertADTHTML text ++ "</a>"
convertADTHTML (InlineCode content) = "<code>" ++ convertADTHTML content ++ "</code>"
convertADTHTML (Footnote content) = 
  let footnoteNumber = extractString content
  in "<sup><a id=\"fn" ++ footnoteNumber ++ "ref\" href=\"#fn" ++ footnoteNumber ++ "\">" ++ footnoteNumber ++ "</a></sup>"
convertADTHTML (StringADT s) = s
-- images
convertADTHTML (Image altText (URL url (QuoteADT caption))) =
  "<img src=\"" ++ extractString url ++  "\" alt=\"" ++ convertADTHTML altText ++ "\" title=\"" ++ convertADTHTML caption ++ "\">"
-- footnote references
convertADTHTML (FootnoteReference (Footnote number) content) =
  let footnoteNumber = extractString number
      footnoteContent = extractString content
  in "<p id=\"fn" ++ footnoteNumber ++ "\">" ++ footnoteContent ++ "</p>"
-- free text
convertADTHTML (FreeText contents) =
  concatMap (\content -> "<p>" ++ convertADTHTML content ++ "</p>") contents
-- headings
convertADTHTML (Header level content) =
  if level >= 1 && level <= 6
    then "<h" ++ show level ++ ">" ++ convertADTHTML content ++ "</h" ++ show level ++ ">"
    else ""
-- block quote
convertADTHTML (BlockQuote content) =
  "<blockquote>" ++ concatMap (\line -> "<p>" ++ convertADTHTML line ++ "</p>") (extractLines content) ++ "</blockquote>"
-- code
convertADTHTML (CodeBlock (Just lang) code) = 
  "<pre><code class=\"language-" ++ lang ++ "\">" ++ code ++ "</code></pre>"
convertADTHTML (CodeBlock Nothing code) = 
  "<pre><code>" ++ code ++ "</code></pre>"
-- ordered lists
convertADTHTML (List items) = "<ol>" ++ convertListItems items ++ "</ol>"
-- tables
convertADTHTML (Table (header:rows)) =
  "<table><thead><tr>" ++ convertHeaderRow header ++ "</tr></thead>" ++
  "<tbody>" ++ concatMap (\row -> "<tr>" ++ convertRow row ++ "</tr>") rows ++ "</tbody></table>"
convertADTHTML _ = ""

-- helper function to extract a string
extractString :: ADT -> String
extractString (StringADT s) = s
extractString _ = ""

-- helper function to extract lines from freetext
extractLines :: ADT -> [ADT]
extractLines (FreeText contents) = contents
extractLines _ = []

-- helper function to convert list items
convertListItems :: [ADT] -> String
convertListItems [] = ""
convertListItems (x:xs) = "<li>" ++ convertADTHTML x ++ "</li>" ++ convertListItems xs

-- helper function to convert a row
convertRow :: [ADT] -> String
convertRow [] = ""
convertRow (x:xs) = "<td>" ++ convertADTHTML x ++ "</td>" ++ convertRow xs

-- helper function to convert a header row
convertHeaderRow :: [ADT] -> String
convertHeaderRow [] = ""
convertHeaderRow (x:xs) = "<th>" ++ convertADTHTML x ++ "</th>" ++ convertHeaderRow xs

--------------------------------
-- Parse Modifiers
--------------------------------

parseSquareBrackets :: Parser ADT
parseSquareBrackets = SquareBrackets <$> (charTok '[' *> parseText ']' <* charTok ']')

parseRoundBrackets :: Parser ADT
parseRoundBrackets = RoundBrackets <$> (charTok '(' *> parseText ')' <* charTok ')')

parseItalic :: Parser ADT
parseItalic = Italic <$> (charTok '_' *> parseText '_' <* charTok '_')

parseBold :: Parser ADT
parseBold = Bold <$> (stringTok "**" *> parseText '*' <* stringTok "**")

parseStrikethrough :: Parser ADT
parseStrikethrough = Strikethrough <$> (stringTok "~~" *> parseText '~' <* stringTok "~~")

parseText :: Char -> Parser ADT
parseText a = (StringADT <$> (spaces *> some (noneof ['*', '~', '_', '[', '`']) <* spaces ))

parseModifier :: Parser ADT
parseModifier = Modifier <$> (spaces *> (parseItalic <|> parseBold <|> parseStrikethrough <|> parseLink <|> parseInlineCode <|> parseFootnote) <* spaces)

-- parseNonModifier :: Parser ADT
-- parseNonModifier = parseText (noneof ['*', '~', '_', '[', '`'])

parseQuoteADT :: Parser ADT
parseQuoteADT = QuoteADT <$> (charTok '\"' *> parseText '\"' <* charTok '\"')

parseLink :: Parser ADT
parseLink = do
  _ <- charTok '['  -- Consume the opening bracket
  linkText <- parseText ']'
  _ <- charTok ']'  -- Consume the closing bracket
  _ <- charTok '('  -- Consume the opening parenthesis
  linkUrl <- parseText ')'
  _ <- charTok ')'  -- Consume the closing parenthesis
  return (Link linkText linkUrl)  -- Construct the Link with the parsed text and URL

parseInlineCode :: Parser ADT
parseInlineCode = InlineCode <$> (stringTok "`" *> parseText '`' <* stringTok "`")

parseNumberString :: Parser ADT
parseNumberString = StringADT <$> some digit

parseFootnote :: Parser ADT
parseFootnote = do
  spaces
  is '['  -- Consume the opening bracket
  is '^'  -- Consume the caret
  n <- parseNumberString  -- Parse the footnote number
  is ']'  -- Consume the closing bracket
  return (Footnote n)  -- Return the parsed footnote

parseURLCaption :: Parser ADT
parseURLCaption = do
  spaces
  _ <- charTok '('  -- Consume the opening parenthesis
  url <- parseText ' '
  caption <- parseQuoteADT
  spaces
  _ <- charTok ')'  -- Consume the closing bracket
  return (URL url caption)  -- Return the parsed URL and caption

parseImage :: Parser ADT
parseImage = do
  spaces
  charTok '!'  -- Consume the exclamation mark
  altText <- parseSquareBrackets
  spaces
  n2 <- parseURLCaption
  spaces
  return (Image altText n2)

parseFootnoteReference :: Parser ADT
parseFootnoteReference = do
  footnote <- parseFootnote
  is ':'
  spaces
  ref <- parseText '\n'
  return (FootnoteReference footnote ref)

parseFreeText :: Parser ADT
parseFreeText = do
  contents <- many (parseModifier <|> parseText '\n')
  return $ FreeText (filterEmptyLines contents)

-- | Helper function to filter out empty lines from FreeText contents
filterEmptyLines :: [ADT] -> [ADT]
filterEmptyLines = filter (\x -> case x of
                                    StringADT s -> not (all (== ' ') s || null s)  -- Remove empty lines
                                    _ -> True)

parseHashHeaders :: Parser ADT
parseHashHeaders = do
  spaces
  hashes <- some (is '#')   -- Parse one or more '#' characters
  is ' '               -- Require at least one space after the hashes
  spaces
  headerText <- parseModifier <|> parseText '\n'
  if length hashes > 6
    then return (ParseErrorADT (UnexpectedChar '#')) -- Fail if there are more than 6 hashes
  else
    return (Header (length hashes) headerText)

-- Parse alternative heading (=== for level 1, --- for level 2)
parseAltHeader :: Char -> Parser ADT
parseAltHeader a = do
  text <- parseModifier <|> parseText '\n'
  (is a)
  some (is a)
  return (case a of 
    '=' -> (Header 1 text)
    '-' -> (Header 2 text)
    _ -> ParseErrorADT(UnexpectedChar a))

parseHeader :: Parser ADT
parseHeader = parseHashHeaders <|> parseAltHeader '=' <|> parseAltHeader '-' 

parseBlockQuote :: Parser ADT
parseBlockQuote = do
  spaces
  _ <- charTok '>'
  spaces            
  blockContent <- some (parseModifier <|> parseText '\n') 
  -- Handle multi-line blockquotes by recursively parsing lines that start with '>'
  moreContent <- many (charTok '>' *> spaces *> (parseModifier <|> parseText '\n'))
  return $ BlockQuote (FreeText (blockContent ++ moreContent))

-- | Parser for newline character
newline :: Parser Char
newline = satisfy (== '\n')  -- Use satisfy to match the newline character

-- | Parse a Markdown code block, starting with ``` and optionally followed by a language identifier.
parseCodeBlock :: Parser ADT
parseCodeBlock = do
  -- Parse optional spaces before the opening backticks
  spaces
  -- Parse the opening backticks (```)
  _ <- stringTok "```"
  
  -- Attempt to parse the language identifier, which may be absent
  lang <- many (noneof "\n") <* newline  -- Use the newline parser

  -- Parse the content of the code block (stopping at the closing backticks)
  codeLines <- many (satisfy (/= '`')) <* endCodeBlock
  
  -- Return the content as a FreeText ADT, including the language identifier if it was present.
  return $ FreeText [StringADT ("Language: " ++ lang ++ "\n" ++ codeLines)]  -- Include language

-- | Parse the closing backticks of a code block (```), followed by optional spaces and a newline.
endCodeBlock :: Parser String
endCodeBlock = stringTok "```" <* spaces <* newline  -- Use the newline parser

-- Parser for a single ordered list item
parseOrderedListItem :: Parser ADT
parseOrderedListItem = do
    num <- some digit <* satisfy (== '.')  -- Read the number before the dot
    _ <- spaces  -- At least one whitespace
    textADT <- parseModifier <|> parseText '\n'  -- Text may include modifiers
    let text = case textADT of
                  StringADT s -> s  -- Extract the string from StringADT
                  _ -> ""  -- Default to empty if it's not StringADT
    return $ StringADT (num ++ ". " ++ text)  -- Combine number and text into StringADT

-- Parser for a sublist item
parseSubListItem :: Parser ADT
parseSubListItem = do
    _ <- stringTok "    "  -- Exactly 4 spaces for sublist items
    item <- parseOrderedListItem  -- Parse the ordered list item as a sublist item
    return item

-- Parser for an ordered list that allows sublists
parseOrderedList :: Parser ADT
parseOrderedList = do
    items <- some (parseOrderedListItem <* newline)  -- Parse at least one ordered list item
    return $ List items  -- Return as a List ADT

-- Parser for a list that can include sublists
parseListWithSubLists :: Parser ADT
parseListWithSubLists = do
    items <- many (parseSubListItem <|> parseOrderedListItem <* newline)
    return $ List items
    
-- Function to parse a single row of the table
parseTableRow :: Parser [ADT]
parseTableRow = do
    _ <- spaces
    firstItem <- parseModifier <|> parseText '\n'  -- Parse first item
    restItems <- many (charTok '|' *> spaces *> (parseModifier <|> parseText '\n'))  -- Parse rest of the items
    _ <- spaces
    return (firstItem : restItems)

-- Function to parse the entire table
parseTable :: Parser ADT
parseTable = do
    headerRow <- parseTableRow
    _ <- charTok '\n'
    separatorRow <- parseTableRow
    _ <- charTok '\n'
    contentRows <- many parseTableRow
    return (Table (headerRow : separatorRow : contentRows))