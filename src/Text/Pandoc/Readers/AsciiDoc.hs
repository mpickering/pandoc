module Text.Pandoc.Readers.AsciiDoc (readAsciiDoc) where


import Text.Pandoc.Builder
import Text.Pandoc.Parsing


readAsciiDoc :: ReaderOptions -> String -> Pandoc
readAsciiDoc = undefined

data AsciiDocState = AsciiDocState {}

type AsciiDoc = ParserT String AsciiDocState Identity

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

attributeList :: A


-- Inlines

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



-- Utility
