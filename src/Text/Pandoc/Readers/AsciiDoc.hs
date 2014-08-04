{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Text.Pandoc.Readers.AsciiDoc where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (Inlines, Blocks)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Parsing hiding (escaped, many, optional, (<|>))
import Data.Default (Default, def)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Applicative
import Data.Char

import Data.Monoid

import Debug.Trace

-- Lenses are defined for these records
data AsciiDocState = AsciiDocState { aLastStrPosition :: Maybe SourcePos }

data AsciiDocLocal = AsciiDocLocal { aInQuoteContext :: QuoteContext }

instance Default AsciiDocState where
  def = AsciiDocState Nothing

instance Default AsciiDocLocal where
  def = AsciiDocLocal NoQuote

type AsciiDoc = ParserT String AsciiDocState (Reader AsciiDocLocal)

readAsciiDoc :: ReaderOptions -> String -> Pandoc
readAsciiDoc os s = runAsciiDoc parseAsciiDoc def s

runAsciiDoc :: AsciiDoc a -> AsciiDocState -> String -> a
runAsciiDoc asdoc st s =  flip runReader def $ readWithM asdoc st (s ++ "\n\n")

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

block :: AsciiDoc Blocks
block = undefined

title :: AsciiDoc Blocks
title = twoLineTitle <|> oneLineTitle

twoLineTitle :: AsciiDoc Blocks
twoLineTitle = try $ do
  tit <- anyLine
  under <- lookAhead anyChar
  guard (under `elem` "=-~&+")
  len <- length <$> many1 (char under)
  let titLen = length title
  guard (len >= titLen - 2 && len <= titlen + 2)
  where
    headers = "=-~&+"



blockMacros :: AsciiDoc Blocks
blockMacros = undefined

list :: AsciiDoc Blocks
list = undefined

delimitedBlock :: AsciiDoc Blocks
delimitedBlock = undefined

table :: AsciiDoc Blocks
table = undefined

horizontalRule :: AsciiDoc Blocks
horizontalRule = atStart *> string "'''" *> many (char '\'') *> blankline *> return B.horizontalRule

-- Inlines

parseInlines :: AsciiDoc Inlines
parseInlines = undefined

inline :: AsciiDoc Inlines
inline = choice
          [ replacement
          , escaped
          , endline
          , emph
          , strong
          , mono
          , quote
          , doubleQuote
          , unquote
          , linebreak ]

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



doubleQuote :: AsciiDoc Inlines
doubleQuote = undefined

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
attributeList = do
  char '['
  classes <- manyTill (posAttr <* spaces <* char ',' <* spaces) (lookAhead namedAttr)
  spaces
  optional $ char ','
  spaces
  kvs <- sepBy namedAttr (char ',')
  char ']'
  return ("", classes, kvs)
  where
    posAttr = try $ do
      notFollowedBy namedAttr
      many1 (noneOf ",]")
    namedAttr = try $ (,) <$> many1 (noneOf "=,") <* char '=' <*> value
    value = try (many1 (noneOf "\",]")) <|> (concat <$> enclosed (char '"') (char '"') (string "\\\"" <|> (:[]) <$> noneOf "\"" ))

escaped :: AsciiDoc Inlines
escaped = try $ B.str . (:[]) <$> (char '\\' *> oneOf specialChars)

attributes :: AsciiDoc Attr
attributes = undefined

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
  fmap (\newPos -> asciistate { aLastStrPosition = newPos })
    (f (aLastStrPosition asciistate))


inQuoteContext :: Lens AsciiDocLocal QuoteContext
inQuoteContext f asciistate  =
  fmap (\newpos -> asciistate { aInQuoteContext = newpos })
    (f (aInQuoteContext asciistate))

-- Instances

instance HasLastStrPosition AsciiDocState where
  setLastStrPos = set lastStrPosition . Just
  getLastStrPos = view lastStrPosition

instance HasQuoteContext st (Reader AsciiDocLocal) where
  getQuoteContext = asks (view inQuoteContext)
  withQuoteContext c = local (set inQuoteContext c)

