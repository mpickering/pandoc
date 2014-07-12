{-# LANGUAGE ViewPatterns #-}

module Text.Pandoc.Readers.Txt2Tags where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder ( Inlines, Blocks, HasMeta(..), (<>)
                                     , trimInlines )
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.LaTeX (inlineCommand, rawLaTeXInline)
import           Text.Pandoc.Shared (splitBy, escapeURI,compactify', compactify'DL)
import Text.Pandoc.Parsing hiding (space)
import           Control.Applicative (  Applicative, pure
                                     , (<$>), (<$), (<*>), (<*), (*>), (<**>) )
import           Control.Monad (foldM, guard, liftM, liftM2, mplus, mzero, when, void)
import           Data.Char (isAlphaNum, toLower)
import           Data.Default
import           Data.List (transpose, intersperse, isPrefixOf, isSuffixOf, intercalate)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isJust, maybe)
import           Data.Monoid (Monoid, mconcat, mempty, mappend)
import           Network.HTTP (urlEncode)
import Debug.Trace
import Data.Sequence (viewr, ViewR(..))
import Network.URI (isURI)
import Control.Monad.Reader
import Text.Parsec.Prim hiding (many)
import Text.Parsec (ParseError)

validChars :: String
validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-"

type T2T = Parser String ParserState 


parseTxt2Tags :: ReaderOptions -> String -> Pandoc
parseTxt2Tags opts s = either (error . show) id (runT2T parseT2T (def {stateOptions = opts}) s)

runT2T :: T2T a -> ParserState -> String ->  Either ParseError a
runT2T t s inp = runParser t s "" inp


parseT2T :: T2T Pandoc
parseT2T = do
  meta <- (Nothing <$ blankline) <|> (Just <$> (count 3 anyLine))
  config <- manyTill setting (notFollowedBy setting)
  let meta = foldr (\(k,v) -> B.setMeta k v) nullMeta config
  body <- mconcat <$>  manyTill block eof
  return $ Pandoc mempty (B.toList body)

type Keyword = String
type Value = String

setting :: T2T (Keyword, Value)
setting = do
  string "%!"
  keyword <- ignoreSpacesCap (many1 alphaNum)
  char ':'
  value <- ignoreSpacesCap (manyTill anyChar (newline))
  return (keyword, value)

-- Blocks

parseBlocks :: T2T Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: T2T Blocks
block = do
  choice 
    [ mempty <$ blanklines
    , title
    , comment
    , hrule
    , verbatim
    , rawBlock
    , taggedBlock
    , quote  
    , list
    , table
    , para
    ] 

title :: T2T Blocks
title = try $ balancedTitle '+' <|> balancedTitle '='

balancedTitle :: Char -> T2T Blocks
balancedTitle c = try $ do
  level <- length <$> many1 (char c) 
  guard (level <= 5) -- Max header level 5
  heading <- manyTill anyChar (lookAhead $ char c)
  count level (char c)
  label <- optionMaybe (enclosed (char '[') (char ']') (oneOf validChars))
  spaces *> newline
  let attr = maybe nullAttr (\x -> (x, [], [])) label
  return $ B.headerWith attr level (B.text heading)

para :: T2T Blocks
para = try (B.para <$> trimInlines . mconcat <$> many1Till inline newline)

commentBlock :: T2T Blocks 
commentBlock = try (blockMarkupArea (anyLine) (const mempty) "%%%")

-- Seperator and Strong line treated the same
hrule :: T2T Blocks 
hrule = try $ do
  spaces 
  line <- manyTill (oneOf "=-_") (newline <|> space)
  guard (length line >= 20)
  B.horizontalRule <$ blankline 
  
  
quote :: T2T Blocks
quote = try $ do
  lookAhead tab
  raw <-  many1 (tab *> optional spaces *> anyLine)
  contents <- parseFromString (parseBlocks) (intercalate "\n" raw ++ "\n\n")
  return $ B.blockQuote contents 

comment :: T2T Blocks
comment = try $ (mempty <$ (char '%' *> anyLine)) <|> commentBlock


-- List Parsing code from Org Reader

list :: T2T Blocks
list = choice [bulletList, orderedList, definitionList]

bulletList :: T2T Blocks
bulletList = B.bulletList . compactify'  
             <$> many1 (listItem bulletListStart)

orderedList :: T2T Blocks
orderedList = B.orderedList . compactify' 
              <$> many1 (listItem orderedListStart)

definitionList :: T2T Blocks
definitionList = do
  listItems <- many1 (listItem definitionListStart)  
  B.definitionList . compactify'DL <$> mapM toDefinitionList listItems

toDefinitionList :: Blocks -> T2T (Inlines, [Blocks]) 
toDefinitionList (B.toList -> (Para is :bs)) = 
  return (B.fromList is, [B.fromList bs])
toDefinitionList b = mzero

genericListStart :: T2T Char
                 -> T2T Int
genericListStart listMarker = try $
  (2+) <$> (length <$> many spaceChar
            <* listMarker <* space <* notFollowedBy space)

-- parses bullet list start and returns its length (excl. following whitespace)
bulletListStart :: T2T  Int
bulletListStart = genericListStart (char '-')

orderedListStart :: T2T Int
orderedListStart = genericListStart (char '+' )

definitionListStart :: T2T Int
definitionListStart = genericListStart (char ':')

-- parse raw text for one list item, excluding start marker and continuations
listItem :: T2T Int
         -> T2T Blocks
listItem start = try $ do
  markerLength <- try start
  firstLine <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  rest <- concat <$> many (listContinuation markerLength)
  parseFromString parseBlocks $ firstLine ++ blank ++ rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> T2T String
listContinuation markerLength = try $
  notFollowedBy' (blankline >> blankline)
  *> (mappend <$> (concat <$> many1 listLine)
              <*> many blankline)
 where listLine = try $ indentWith markerLength *> anyLineNewline

anyLineNewline :: T2T String
anyLineNewline = (++ "\n") <$> anyLine

indentWith :: Int -> T2T String
indentWith n = count n space

-- Table

table :: T2T Blocks
table = try $ do
  header <- fmap snd <$> option mempty (headerRow)
  rows <- many1 (many comment *> tableRow)
  let columns = transpose rows
  let ncolumns = length columns
  let aligns = map (foldr findAlign AlignDefault) (map (map fst) columns)
  let rows' = map (map snd) rows
  let rowsPadded = map (pad (maximum (map length rows'))) rows'
  return $ B.table mempty 
                    (zip aligns (replicate ncolumns (1.0 / fromIntegral ncolumns))) 
                      header rowsPadded

pad :: Monoid a => Int -> [a] -> [a]
pad n xs = xs ++ (replicate (length xs - n) mempty)

findAlign :: Alignment -> Alignment -> Alignment 
findAlign AlignDefault x = AlignDefault
findAlign x AlignDefault = x
findAlign x y 
  | x == y = x
  | otherwise = AlignDefault 

headerRow :: T2T [(Alignment, Blocks)]
headerRow = genericRow (string "||")

tableRow :: T2T [(Alignment, Blocks)]
tableRow = genericRow (char '|')

genericRow :: T2T a -> T2T [(Alignment, Blocks)]
genericRow start = try $ do 
  spaces *> start
  manyTill tableCell newline <?> "genericRow"
 

tableCell :: T2T (Alignment, Blocks)
tableCell = try $ do
  leftSpaces <- length <$> many1 space
  content <- (many1Till inline (try $ lookAhead (void newline <|> (many1 space *> endOfCell))))
  a <- getInput
  traceShow ("tableCell", a, leftSpaces) (return ())
  rightSpaces <- length <$> many space 
  let align = 
        case compare leftSpaces rightSpaces of
              LT -> AlignLeft
              EQ -> AlignCenter
              GT -> AlignRight
  trace "here" (return ())
  a <- getInput
  traceShow a (return ())
  endOfCell
  return $ (align, B.para (mconcat content))

endOfCell :: T2T () 
endOfCell = try (skipMany1 $ char '|') <|> ( () <$ lookAhead newline)

-- Raw area

verbatim :: T2T Blocks 
verbatim = genericBlock anyLine B.codeBlock "```"

rawBlock :: T2T Blocks
rawBlock = genericBlock anyLine (B.para . B.str) "\"\"\""

taggedBlock :: T2T Blocks 
taggedBlock = do
  target <- getTarget
  genericBlock anyLine (B.rawBlock target) "'''"

-- Generic

genericBlock p f s = blockMarkupArea p f s <|> blockMarkupLine p f s 

blockMarkupArea :: Monoid a => (T2T a) -> (a -> Blocks) -> String -> T2T Blocks
blockMarkupArea p f s = try $ (do
  string s *> blankline
  f . mconcat <$> (manyTill p (eof <|> void (string s *> blankline))))

blockMarkupLine :: T2T a -> (a -> Blocks) -> String -> T2T Blocks
blockMarkupLine p f s = try (f <$> (string s *> p))

-- Inline

inline :: T2T Inlines
inline = do
  choice 
    [ link
    , image
    , bold
    , underline
    , strike
    , italic
    , code
    , str
    , symbol
    , whitespace
    ]


bold :: T2T Inlines
bold = inlineMarkup inline B.strong '*'

underline :: T2T Inlines
underline = inlineMarkup inline B.emph '_'

strike :: T2T Inlines
strike = inlineMarkup inline B.strikeout '-'

italic :: T2T Inlines
italic = inlineMarkup inline B.emph '/'

code :: T2T Inlines
code = inlineMarkup ((:[]) <$> anyChar) B.code '`'

raw :: T2T Inlines
raw = inlineMarkup ((:[]) <$> anyChar) B.text '"'

tagged :: T2T Inlines
tagged = do
  target <- getTarget
  inlineMarkup ((:[]) <$> anyChar) (B.rawInline target) '\''

inlineMarkup :: Monoid a => (T2T a) -> (a -> Inlines) -> Char -> T2T Inlines
inlineMarkup p f c = try $ do
  start <- many1 (char c)
  guard (length start >= 2)
  lookAhead (noneOf [' ', c])
  rawBody <-  manyTill anyChar (try $ lookAhead (noneOf " " >> string [c,c] ))
  lastChar <- anyChar 
  end <- many1 (char c)
  s <- getState
  let parser inp = runT2T (mconcat <$> many p) s inp 
  let start' = either (const mempty) id (parser (drop 2 start))
  let Right body = parser (rawBody ++ [lastChar])
  let end' = either (const mempty) id (parser (drop 2 start))
  return $ f (start' <> body <> end')

titleLink :: T2T Inlines
titleLink = try $ do
  char '['
  tokens <- words <$> manyTill (noneOf " ") (char ']')
  let l = last tokens 
  let alt = B.text $ concat (intersperse " " (init tokens))
  return $ B.link (escapeURI l) "" alt

imageLink :: T2T Inlines
imageLink = try $ do 
  char '['
  body <- image
  spaces
  l <- manyTill anyChar (char ']')
  return (B.link l "" body)

link :: T2T Inlines
link = imageLink <|> titleLink

image :: T2T Inlines
image =  try $ (\url -> B.image url "" mempty) <$> enclosed (char '[') (char ']') (noneOf " ")

-- Characters used in markup
specialChars :: String
specialChars = "%*-_/|:+"

tab :: T2T Char
tab = char '\t'

space :: T2T Char
space = char ' '

str :: T2T Inlines 
str = try $ B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")

whitespace :: T2T Inlines 
whitespace = try $ B.space <$ oneOf "\n\r "

symbol :: T2T Inlines
symbol = B.str . (:[]) <$> oneOf specialChars 

-- Fix Up

--isURI

-- Utility

getTarget :: T2T String
getTarget = do
  mv <- lookupMeta "target" . stateMeta <$> getState 
  let MetaString target = fromMaybe (MetaString "html") mv
  return target

atStart :: T2T ()
atStart = (sourceColumn <$> getPosition) >>= guard . (== 0)

ignoreSpacesCap :: T2T String -> T2T String
ignoreSpacesCap p = map toLower <$> (spaces *> p <* spaces)

