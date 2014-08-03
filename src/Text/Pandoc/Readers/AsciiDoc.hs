module Text.Pandoc.Readers.AsciiDoc where

import Text.Pandoc.Builder (Inlines, Blocks)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Parsing


readAsciiDoc :: ReaderOptions -> String -> Pandoc
readAsciiDoc = undefined

data AsciiDocState = AsciiDocState {}

instance Default AsciiDocState where
  def = AsciiDocState

type AsciiDoc = ParserT String AsciiDocState Identity

readAsciiDoc :: ReaderOptions -> String -> Pandoc
readAsciiDoc = readWith parseAsciiDoc def (s ++ "\n\n")

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
title = undefined

blockMacros :: AsciiDoc Blocks
blockMacros = undefined

list :: AsciiDoc Blocks
list = undefined

delimitedBlock :: AsciiDoc Blocks
delimitedBlock = undeined

table :: AsciiDoc Table
table = undefined

horizontalRule :: AsciiDoc Blocks
horizontalRule = undefined

-- Inlines

parseInlines :: AsciiDoc Inlines
parseInlines = undefined

inline :: AsciiDoc Inlines
inline = undefined

specialCharacter :: AsciiDoc Inlines
specialCharacter = undefined

quote :: AsciiDoc Inlines
quote = undefined

specialWord :: AsciiDoc Inlines
specialWord = undefined

replacement :: AsciiDoc Inlines
replacement = undefined

inlineMacro :: AsciiDoc Inlines
inlineMacro = undefined

-- Formatting
emph :: AsciiDoc Inlines
emph = quotedText '_' B.emph

strong :: AsciiDoc Inlines
strong = quotedText '*' B.strong

mono :: AsciiDoc Inlines
mono = quotedText '+' B.code

quote :: AsciiDoc Inlines
quote = undefined

doubleQuote :: AsciiDoc Inlines
doubleQuote = undefined

unquote :: AsciiDoc Inlines
unquote = quotedText '#' id

-- Constrained as defined here-
-- https://groups.google.com/forum/#!searchin/asciidoc/constrained/asciidoc/-jgLVHILFRg/3K-K4gXoFosJ
-- Surrounded by any character in the set [^a-zA-Z0-9_]
quotedText :: Char -> (Inlines -> Inlines) -> AsciiDoc Inlines
quotedText c f = try $ do
  let boundary x = not (isAlphaNum x || x == '_')
  b <- B.text . (:[]) <$> (satisfy boundary) <|> (atStart *> return mempty)
  r <- enclosed (char c) (char c) (inline)
  lookAhead (satisfy boundary)
  return (b <> f r)

escape :: AsciiDoc Inlines
escape = undefined

attributes :: AsciiDoc Attr
attributes = undefined

superscript :: AsciiDoc Inlines
superscript = undefined

subscript :: AsciiDoc Inlines
subscript = undefined

linebreak :: AsciiDoc Inlines
linebreak = undefined

replacement :: AsciiDoc Inlines
replacement = undefined

specialWords :: AsciiDoc Inlines
specialWords = undefined

space :: AsciiDoc Inlines
space = undefined

str :: AsciiDoc Inlines
str = undefined

symbol :: AsciiDoc Inlines
symbol = undefined


-- Utility

atStart :: AsciiDoc ()
atStart = getPosition >>= guard . (== 1) . sourceColumn

specialChars = [Char]
specialChars = "^*/+-._="


