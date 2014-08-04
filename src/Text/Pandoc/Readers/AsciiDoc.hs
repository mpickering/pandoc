{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Text.Pandoc.Readers.AsciiDoc where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Parsing hiding (escaped, many, optional, (<|>))
import Data.Default (Default, def)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Applicative
import Data.Char

import Data.List
import Data.Monoid
import Data.Maybe (fromMaybe)

import Debug.Trace

-- Lenses are defined for these records
data AsciiDocState = AsciiDocState { aLastStrPosition :: Maybe SourcePos }

data AsciiDocLocal = AsciiDocLocal { aInQuoteContext :: QuoteContext
                                  ,  aInVerse :: Bool }

instance Default AsciiDocState where
  def = AsciiDocState Nothing

instance Default AsciiDocLocal where
  def = AsciiDocLocal NoQuote False

type AsciiDoc = ParserT String AsciiDocState (Reader AsciiDocLocal)

readAsciiDoc :: ReaderOptions -> String -> Pandoc
readAsciiDoc os s = runAsciiDoc parseAsciiDoc def (s ++ "\n\n")

runAsciiDoc :: AsciiDoc a -> AsciiDocState -> String -> a
runAsciiDoc asdoc st s =  flip runReader def $ readWithM asdoc st s

parseAsciiDoc :: AsciiDoc Pandoc
parseAsciiDoc = do
  header
  many1 block
  return undefined

-- Prelude

header :: AsciiDoc Meta
header = do
  documentTitle
  author
  revision
  blankline
  return undefined

documentTitle :: AsciiDoc ()
documentTitle = undefined

author :: AsciiDoc ()
author = undefined

revision :: AsciiDoc ()
revision = undefined

comment :: AsciiDoc ()
comment = undefined

attributeEntry :: AsciiDoc ()
attributeEntry = undefined




-- Blocks

parseBlocks :: AsciiDoc Blocks
parseBlocks = undefined

appendAttr :: Attr -> Attr -> Attr
appendAttr (id1, cs, kvs) (_, cs', kvs') = (id1, cs ++ cs', kvs ++ kvs')

block :: AsciiDoc Blocks
block = do
  as@(_, cs, _) <- foldr appendAttr nullAttr <$> many (attributeList <* blankline)
  let cons = if as == nullAttr then id else B.divWith as
  traceShow (cons mempty) (return ())
  b <- choice
        [ title
        , literal
        , admonition
        , delimitedBlocks
        , local (set inVerse ("verse" `elem` cs)) para
        ]
  return $ cons b

title :: AsciiDoc Blocks
title = twoLineTitle <|> oneLineTitle

twoLineTitle :: AsciiDoc Blocks
twoLineTitle = try $ do
  text <- anyLine
  under <- lookAhead anyChar
  guard (under `elem` "=-~&+")
  len <- length <$> many1 (char under)
  let titLen = length text
  guard (len >= titLen - 2 && len <= titLen + 2)
  let level = fromMaybe (error "Invalid header underline") (findIndex (== under) headerChars)
  titleText <- parseFromString parseInlines text
  return $ B.header level titleText
  where
    headerChars = "=-~&+"

oneLineTitle :: AsciiDoc Blocks
oneLineTitle = try $ do
  level <- (\x -> length x - 1) <$> many1 (char '=')
  guard (level <= 4)
  space
  let titleEnd = many (char '=') *> blankline
  titleText <- trimInlines . mconcat <$> many1 (notFollowedBy titleEnd *> inline)
  return $ B.header level titleText

blockId :: AsciiDoc Blocks
blockId = try $ do
  divId <- enclosed (string "[[") (string "]]") (noneOf "]")
  blankline
  return $ B.divWith (divId, [], []) mempty

para :: AsciiDoc Blocks
para = try $ do
  ils <- parseInlines
  nl <- option False (True <$ newline)
  option (B.plain ils) (guard nl >> notFollowedBy listStart >> return (B.para ils))
  where
    listStart = char '*'

admonition :: AsciiDoc Blocks
admonition = try $ do
    choice (map mkAdmonParser admonitions)
  where
    mkAdmonParser s = try $ string s *> char ':' *> spaces *> (B.divWith ("", [s], []) <$> para)

admonitions :: [String]
admonitions = ["NOTE", "CAUTION", "TIP", "IMPORTANT", "WARNING"]

literal :: AsciiDoc Blocks
literal = try $ do
  ind <- length <$> lookAhead (many1 spaceChar)
  B.codeBlock . unlines <$> many1 (count ind spaceChar *> anyLine)

delimitedBlocks :: AsciiDoc Blocks
delimitedBlocks =
  lookAhead (count 4 (oneOf dchars)) *>  -- Not sure if this optimisation is worthwhile
  choice
    [ commentBlock
    , passthroughBlock
    , listingBlock
    , literalBlock
    , sidebarBlock
    , quoteBlock
    , exampleBlock ]
  where
    dchars = "/+-._=*"

commentBlock :: AsciiDoc Blocks
commentBlock = delimitedBlock '/' anyLine (const mempty)

passthroughBlock :: AsciiDoc Blocks
passthroughBlock = delimitedBlock '+' inline B.para

listingBlock :: AsciiDoc Blocks
listingBlock = delimitedBlock '-' anyLine B.codeBlock

literalBlock :: AsciiDoc Blocks
literalBlock = delimitedBlock '.' anyLine B.codeBlock

sidebarBlock :: AsciiDoc Blocks
sidebarBlock = delimitedBlock '*' block (B.para . B.note)

quoteBlock :: AsciiDoc Blocks
quoteBlock = delimitedBlock '_' inline ((B.divWith ("", ["quote"], [])) . B.para)

exampleBlock :: AsciiDoc Blocks
exampleBlock = delimitedBlock '=' inline ((B.divWith ("", ["example"], [])) . B.para)

delimitedBlock :: Monoid a => Char -> (AsciiDoc a) -> (a -> Blocks) -> AsciiDoc Blocks
delimitedBlock fence parser constructor = try $ do
  count 4 (char fence) *> many (char fence) *> blankline
  r <- mconcat <$> manyTill parser (count 4 (char fence) *> many (char fence) *> blankline)
  return (constructor r)

openBlock :: AsciiDoc Blocks
openBlock = try $ do
  string "--" *> blankline
  parseBlocks *> string "--" *> blankline


blockMacros :: AsciiDoc Blocks
blockMacros = undefined

-- Lists

list :: AsciiDoc Blocks
list = bulletList <|> numberedList <|> labeledList <|> calloutList

bulletList :: AsciiDoc Blocks
bulletList = B.bulletList . compactify'
             <$> many1 (listItem bulletListStart parseBlocks)


numberedList :: AsciiDoc Blocks
numberedList = undefined

labeledList :: AsciiDoc Blocks
labeledList = undefined

calloutList :: AsciiDoc Blocks
calloutList = undefined

-- Table

table :: AsciiDoc Blocks
table = undefined

horizontalRule :: AsciiDoc Blocks
horizontalRule = atStart *> string "'''" *> many (char '\'') *> blankline *> return B.horizontalRule

-- Inlines

parseInlines :: AsciiDoc Inlines
parseInlines = trimInlines . mconcat <$> many1 inline

inline :: AsciiDoc Inlines
inline = choice
          [ replacement
          , escaped
          , emph
          , strong
          , mono
          , quote
          , unquote
          , str
          , linebreak
          , whitespace ]

specialCharacter :: AsciiDoc Inlines
specialCharacter = undefined

specialWord :: AsciiDoc Inlines
specialWord = undefined

inlineMacro :: AsciiDoc Inlines
inlineMacro = undefined

-- Formatting
emph :: AsciiDoc Inlines
emph = quotedText '_' inline B.emph

strong :: AsciiDoc Inlines
strong = quotedText '*' inline B.strong

mono :: AsciiDoc Inlines
mono = quotedText '+' ((:[]) <$> anyChar) B.code

quote :: AsciiDoc Inlines
quote = quoted inline

unquote :: AsciiDoc Inlines
unquote = quotedText '#' inline id


quotedText :: Monoid a => Char -> AsciiDoc a -> (a -> Inlines) -> AsciiDoc Inlines
quotedText c p f = consText c p f  <|> unconsText [c, c] p f

-- Constrained as defined here-
-- https://groups.google.com/forum/#!searchin/asciidoc/constrained/asciidoc/-jgLVHILFRg/3K-K4gXoFosJ
-- Surrounded by any character in the set [^a-zA-Z0-9_]
consText :: Monoid a => Char -> AsciiDoc a -> (a -> Inlines) -> AsciiDoc Inlines
consText c p f = try $ do
  let boundary x = not (isAlphaNum x || x == '_')
  b <- (atStart *> return mempty) <|> (mempty <$ lookAhead attributeList) <|> (B.text . (:[]) <$> satisfy boundary)
  cons <- maybe id B.spanWith <$> optionMaybe attributeList
  r <- mconcat <$> enclosed (char c) (char c) p
  lookAhead (satisfy boundary)
  return (b <> cons (f r))

unconsText :: Monoid a => String -> AsciiDoc a -> (a -> Inlines) -> AsciiDoc Inlines
unconsText bound p f = try $ do
  f <$> mconcat <$> enclosed (string bound) (string bound) p


attributeList :: AsciiDoc Attr
attributeList = try $ do
  char '['
  let sep = spaces *> optional (char ',') *> spaces
  classes <- manyTill (posAttr <* sep) (lookAhead (void (try (lookAhead namedAttr)) <|> void ((char ']'))))
  traceShow "here" (return ())
  kvs <- sepBy namedAttr (char ',')
  traceShow "fail" (return ())
  a <- getInput
  traceShow (a, classes, kvs) (return ())
  char ']'
  return ("", classes, kvs)
  where
    posAttr = try $ many1 (noneOf ",]")
    namedAttr = try $ (,) <$> many1 (noneOf "]=,") <* char '=' <*> value
    value = try (many1 (noneOf "\",]")) <|> (concat <$> enclosed (char '"') (char '"') (string "\\\"" <|> (:[]) <$> noneOf "\"" ))

escaped :: AsciiDoc Inlines
escaped = try $ B.str . (:[]) <$> (char '\\' *> oneOf specialChars)

superscript :: AsciiDoc Inlines
superscript = unconsText "^" inline B.superscript

subscript :: AsciiDoc Inlines
subscript = unconsText "~" inline B.subscript

linebreak :: AsciiDoc Inlines
linebreak = B.linebreak <$ try (space *> char '+')

replacement :: AsciiDoc Inlines
replacement = choice (map (uncurry replace) replacements)
  where
    replace s alt = try $ string s *> return (B.str alt)


replacements :: [(String, String)]
replacements = [ ("(C)", "\x00A9")
               , ("(TM)", "\x2122")
               , ("(R)",  "\x00AE")
               , ("--", "\x2013")
               , ("---", "\x2014")
               , ("->", "\x2192")
               , ("<-", "\x2190")
               , ("=>", "\x21D2")
               , ("<=", "\x21D0")]

-- Unimplemented until config reading is added
specialWords :: AsciiDoc Inlines
specialWords = undefined

spacec :: AsciiDoc Inlines
spacec = undefined

endline :: AsciiDoc Inlines
endline = undefined

str :: AsciiDoc Inlines
str = B.str <$> many1 (noneOf (specialChars ++ " \n\r\t"))

whitespace :: AsciiDoc Inlines
whitespace = B.space <$ oneOf " \n\r\t"

symbol :: AsciiDoc Inlines
symbol = B.str . (:[]) <$> oneOf specialChars


-- Utility

atStart :: AsciiDoc ()
atStart = getPosition >>= guard . (== 1) . sourceColumn

specialChars :: [Char]
specialChars = "^*/+-._='#"

-- Lenses

type Lens s a = Functor f => (a -> f a) -> s -> f s

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln (Identity . f) s

view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

set :: Lens s a -> a -> s -> s
set ln x = over ln (const x)

lastStrPosition :: Lens AsciiDocState (Maybe SourcePos)
lastStrPosition f asciistate  =
  fmap (\newpos -> asciistate { aLastStrPosition = newpos })
    (f (aLastStrPosition asciistate))

inQuoteContext :: Lens AsciiDocLocal QuoteContext
inQuoteContext f asciistate  =
  fmap (\newpos -> asciistate { aInQuoteContext = newpos })
    (f (aInQuoteContext asciistate))

inVerse :: Lens AsciiDocLocal Bool
inVerse f asciistate  =
  fmap (\newpos -> asciistate { aInVerse = newpos })
    (f (aInVerse asciistate))
-- Instances

instance HasLastStrPosition AsciiDocState where
  setLastStrPos = set lastStrPosition . Just
  getLastStrPos = view lastStrPosition

instance HasQuoteContext st (Reader AsciiDocLocal) where
  getQuoteContext = asks (view inQuoteContext)
  withQuoteContext c = local (set inQuoteContext c)

