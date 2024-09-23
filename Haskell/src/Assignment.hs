module Assignment (markdownParser, convertADTHTML) 
where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        
import           Parser 
import          Control.Applicative (Alternative (..))          

data ADT =
  Empty |
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
  Code ADT |
  OrderedList [(Int, ADT)] |
  Table [[ADT]]


  deriving (Show, Eq)

markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = "IMPLEMENT_THIS"

--------------------------------
-- Parse Modifiers
--------------------------------

parseSquareBrackets :: Parser ADT
parseSquareBrackets = SquareBrackets <$> (charTok '[' *> parseText ']' <* charTok ']')

parseRoundBrackets :: Parser ADT
parseRoundBrackets = RoundBrackets <$> (charTok '(' *> parseText ')' <* charTok ')')

parsePlainText :: Parser ADT
parsePlainText = do
  spaces
  content <- many1 (noneof "|")
  spaces
  return $ StringADT content

parseModifier :: Parser ADT
parseModifier = Modifier <$> (spaces *> (parseItalic <|> parseBold <|> parseStrikethrough <|> parseLink <|> parseInlineCode <|> parseFootnote <|> parseImage <|> parseFootnoteReference) <* spaces)

parseItalic :: Parser ADT
parseItalic = Italic <$> (charTok '_' *> parseText '_' <* charTok '_')

parseBold :: Parser ADT
parseBold = Bold <$> (stringTok "**" *> parseText '*' <* stringTok "**")

parseStrikethrough :: Parser ADT
parseStrikethrough = Strikethrough <$> (stringTok "~~" *> parseText '~' <* stringTok "~~")

parseText :: Char -> Parser ADT
parseText a =  StringADT <$> (spaces *> some (isNot a) <* spaces )

parseNonModifier :: Parser ADT
parseNonModifier = StringADT <$> (spaces *> some (noneof(['_', '*', '[', ']', '(', ')', '`', '!', '^', ':'])) <* spaces)

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
  is ':'  -- Consume the opening bracket
  spaces
  ref <- parseText '\n'
  return (FootnoteReference footnote ref)



parseFreeText :: Parser ADT
parseFreeText = FreeText <$> (some (spaces *> (parseModifier <|> parseNonModifier) <* spaces))

-- Helper to parse a sequence of hashes (i.e., # for Heading level 1 to 6)
parseHashHeader :: Parser Int
parseHashHeader = do
  hashes <- many1 (char '#')   -- Parse one or more '#' characters
  spaces                        -- Require at least one space after the hashes
  let level = length hashes    -- The number of '#' represents the heading level
  if level > 6 then fail "Invalid header level" else return level

-- Parse alternative heading (=== for level 1, --- for level 2)
parseAltHeader :: Parser Int
parseAltHeader = do
  headingText <- many1 anyChar   -- Parse the text line (content of the heading)
  newline
  altLine <- many1 (oneOf "= -") -- Parse the alternative line (either === or ---)
  let level = if head altLine == '=' then 1 else 2
  return level

-- Combine both hash-based and alternative header parsers
parseHeaderLevel :: Parser Int
parseHeaderLevel = try parseHashHeader <|> parseAltHeader

-- Parser for the content of the heading (handling modifiers, brackets, quotes, etc.)
parseHeaderContent :: Parser ADT
parseHeaderContent = FreeText <$> many1 (parseModifier <|> parseSquareBrackets <|> parseRoundBrackets <|> parseQuoteADT <|> parseNonModifier)

-- Top-level parser for a heading
parseHeader :: Parser ADT
parseHeader = do
  level <- parseHeaderLevel     -- Parse the header level (1-6)
  content <- parseHeaderContent -- Parse the content of the header
  return $ Header level content -- Return the Header ADT

-- parseHeader1 :: a

-- parseHeader1 = undefined
-- parseHeader :: a
-- parseHeader = Header <$> ((parseHeader1 <|> parseHeader2) *> parseModifier)

parseCodeBlock :: Parser ADT
parseCodeBlock = do
  _ <- string "```"
  lang <- optionMaybe (many1 alphaNum)
  newline
  codeLines <- manyTill anyChar (try (newline >> string "```"))
  return $ Code (StringADT codeLines)

-- Parser for a list item, which starts with a number followed by a period and a space
parseListItem :: Parser (Int, ADT)
parseListItem = do
  num <- read <$> many1 digit
  _ <- char '.'
  _ <- space
  content <- many1 (parseBold <|> parseItalic <|> parsePlainText)
  return (num, FreeText content)

-- Parser for sublist items (which are indented with exactly 4 spaces)
parseSubListItem :: Parser (Int, ADT)
parseSubListItem = do
  _ <- count 4 (char ' ')      -- Exactly 4 spaces before sublist items
  parseListItem

parseOrderedList :: Parser ADT
parseOrderedList = do
  items <- some parseListItem
  subItems <- many (try parseSubListItem)
  let allItems = items ++ subItems
  return $ OrderedList allItems

parseTableCell :: Parser ADT
parseTableCell = try parseBold <|> try parseItalic <|> parsePlainText

parseTableRow :: Parser [ADT]
parseTableRow = do
  _ <- char '|'               -- Start of the row
  cells <- sepBy parseTableCell (char '|')
  _ <- char '|'               -- End of the row
  newline
  return cells

parseHeaderSeparator :: Int -> Parser ()
parseHeaderSeparator numColumns = do
  _ <- char '|'
  sepBy (count 3 (char '-') *> many (char '-')) (char '|') -- At least 3 dashes per column
  _ <- char '|'
  newline
  return ()

parseTable :: Parser ADT
parseTable = do
  header <- parseTableRow
  let numColumns = length header
  _ <- parseHeaderSeparator numColumns
  rows <- many1 (parseTableRowWithColumns numColumns)
  return $ Table (header : rows)

parseTableRowWithColumns :: Int -> Parser [ADT]
parseTableRowWithColumns numColumns = do
  row <- parseTableRow
  if length row == numColumns
    then return row
    else fail "Row has a different number of columns than the header."