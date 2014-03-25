{-
Copyright (C) 2010 Paul Rivier <paul*rivier#demotera*com> | tr '*#' '.@'

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Textile
   Copyright   : Copyright (C) 2010-2012 Paul Rivier and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Paul Rivier <paul*rivier#demotera*com>
   Stability   : alpha
   Portability : portable

Conversion from Textile to 'Pandoc' document, based on the spec
available at http://redcloth.org/textile.

Implemented and parsed:
 - Paragraphs
 - Code blocks
 - Lists
 - blockquote
 - Inlines : strong, emph, cite, code, deleted, superscript,
   subscript, links
 - footnotes
 - HTML-specific and CSS-specific attributes on headers

Left to be implemented:
 - dimension sign
 - all caps
 - continued blocks (ex bq..)

TODO : refactor common patterns across readers :
 - more ...

-}


module Text.Pandoc.Readers.Textile ( readTextile) where

import Debug.Trace
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing 
import Text.Pandoc.Readers.HTML ( htmlTag, isInlineTag, isBlockTag )
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.HTML.TagSoup (parseTags, innerText, fromAttrib, Tag(..))
import Text.HTML.TagSoup.Match
import Data.List ( intercalate )
import Data.Char ( digitToInt, isUpper, isDigit )
import Control.Monad ( guard, liftM )
import Control.Applicative ((<$>), (*>), (<*), (<$))
import Data.Monoid

-- | Parse a Textile text and return a Pandoc document.
readTextile :: ReaderOptions -- ^ Reader options
            -> String       -- ^ String to parse (assuming @'\n'@ line endings)
            -> Pandoc
readTextile opts s =
  (readWith parseTextile) def{ stateOptions = opts } (' ' : s ++ "\n\n")


-- | Generate a Pandoc ADT from a textile document
parseTextile :: Parser [Char] ParserState Pandoc
parseTextile = do
  -- textile allows raw HTML and does smart punctuation by default
  oldOpts <- stateOptions `fmap` getState
  updateState $ \state -> state{ stateOptions =
                                   oldOpts{ readerSmart = True
                                          , readerParseRaw = True
                                          , readerOldDashes = True
                                          } }
  many blankline
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys/notes were...
  let firstPassParser = noteBlock <|> lineClump
  manyTill firstPassParser eof >>= setInput . concat
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  blocks <- parseBlocks
  return $ Pandoc nullMeta blocks -- FIXME

noteMarker :: Parser [Char] ParserState [Char]
noteMarker = skipMany spaceChar >> string "fn" >> manyTill digit (char '.')

noteBlock :: Parser [Char] ParserState [Char]
noteBlock = try $ do
  startPos <- getPosition
  ref <- noteMarker
  optional blankline
  contents <- liftM unlines $ many1Till anyLine (blanklines <|> noteBlock)
  endPos <- getPosition
  let newnote = (ref, contents ++ "\n")
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

-- | Parse document blocks
parseBlocks :: Parser [Char] ParserState [Block]
parseBlocks = manyTill block eof

-- | Block parsers list tried in definition order
blockParsers :: [Parser [Char] ParserState Block]
blockParsers = [ codeBlock
               , header
               , blockQuote
               , hrule
               , commentBlock
               , anyList
               , rawHtmlBlock
               , rawLaTeXBlock'
               , maybeExplicitBlock "table" table
               , maybeExplicitBlock "p" para
               ]

-- | Any block in the order of definition of blockParsers
block :: Parser [Char] ParserState Block
block = choice blockParsers <?> "block"

commentBlock :: Parser [Char] ParserState Block
commentBlock = try $ do
  string "###."
  manyTill anyLine blanklines
  return Null

codeBlock :: Parser [Char] ParserState Block
codeBlock = codeBlockBc <|> codeBlockPre

codeBlockBc :: Parser [Char] ParserState Block
codeBlockBc = try $ do
  string "bc. "
  contents <- manyTill anyLine blanklines
  return $ CodeBlock ("",[],[]) $ unlines contents

-- | Code Blocks in Textile are between <pre> and </pre>
codeBlockPre :: Parser [Char] ParserState Block
codeBlockPre = try $ do
  (t@(TagOpen _ attrs),_) <- htmlTag (tagOpen (=="pre") (const True))
  result' <- (innerText . parseTags) `fmap` -- remove internal tags
               manyTill anyChar (htmlTag (tagClose (=="pre")))
  optional blanklines
  -- drop leading newline if any
  let result'' = case result' of
                      '\n':xs -> xs
                      _       -> result'
  -- drop trailing newline if any
  let result''' = case reverse result'' of
                       '\n':_ -> init result''
                       _      -> result''
  let classes = words $ fromAttrib "class" t
  let ident = fromAttrib "id" t
  let kvs = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ CodeBlock (ident,classes,kvs) result'''

-- | Header of the form "hN. content" with N in 1..6
header :: Parser [Char] ParserState Block
header = try $ do
  char 'h'
  level <- digitToInt <$> oneOf "123456"
  attr <- attributes
  char '.'
  lookAhead whitespace
  name <- normalizeSpaces . B.toList . mconcat <$> manyTill inline blockBreak
  attr' <- registerHeader attr (B.fromList name)
  return $ Header level attr' name

-- | Blockquote of the form "bq. content"
blockQuote :: Parser [Char] ParserState Block
blockQuote = try $ do
  string "bq" >> attributes >> char '.' >> whitespace
  BlockQuote . singleton <$> para

-- Horizontal rule

hrule :: Parser [Char] st Block
hrule = try $ do
  skipSpaces
  start <- oneOf "-*"
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return HorizontalRule

-- Lists handling

-- | Can be a bullet list or an ordered list. This implementation is
-- strict in the nesting, sublist must start at exactly "parent depth
-- plus one"
anyList :: Parser [Char] ParserState Block
anyList = try $ anyListAtDepth 1 <* blanklines

-- | This allow one type of list to be nested into an other type,
-- provided correct nesting
anyListAtDepth :: Int -> Parser [Char] ParserState Block
anyListAtDepth depth = choice [ bulletListAtDepth depth,
                                orderedListAtDepth depth,
                                definitionList ]

-- | Bullet List of given depth, depth being the number of leading '*'
bulletListAtDepth :: Int -> Parser [Char] ParserState Block
bulletListAtDepth depth = try $ BulletList <$> many1 (bulletListItemAtDepth depth)

-- | Bullet List Item of given depth, depth being the number of
-- leading '*'
bulletListItemAtDepth :: Int -> Parser [Char] ParserState [Block]
bulletListItemAtDepth = genericListItemAtDepth '*'

-- | Ordered List of given depth, depth being the number of
-- leading '#'
orderedListAtDepth :: Int -> Parser [Char] ParserState Block
orderedListAtDepth depth = try $ do
  items <- many1 (orderedListItemAtDepth depth)
  return (OrderedList (1, DefaultStyle, DefaultDelim) items)

-- | Ordered List Item of given depth, depth being the number of
-- leading '#'
orderedListItemAtDepth :: Int -> Parser [Char] ParserState [Block]
orderedListItemAtDepth = genericListItemAtDepth '#'

-- | Common implementation of list items
genericListItemAtDepth :: Char -> Int -> Parser [Char] ParserState [Block]
genericListItemAtDepth c depth = try $ do
  count depth (char c) >> attributes >> whitespace
  p <- B.toList . mconcat <$> many listInline
  newline
  sublist <- option [] (singleton <$> anyListAtDepth (depth + 1))
  return (Plain p : sublist)

-- | A definition list is a set of consecutive definition items
definitionList :: Parser [Char] ParserState Block
definitionList = try $ DefinitionList <$> many1 definitionListItem

-- | List start character.
listStart :: Parser [Char] st Char
listStart = oneOf "*#-"

listInline :: Parser [Char] ParserState B.Inlines
listInline = try (notFollowedBy newline >> inline)
         <|> try (endline <* notFollowedBy listStart)

-- | A definition list item in textile begins with '- ', followed by
-- the term defined, then spaces and ":=". The definition follows, on
-- the same single line, or spaned on multiple line, after a line
-- break.
definitionListItem :: Parser [Char] ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  string "- "
  term <- B.toList . mconcat <$> many1Till inline (try (whitespace >> string ":="))
  def' <- multilineDef <|> inlineDef
  return (term, def')
  where inlineDef :: Parser [Char] ParserState [[Block]]
        inlineDef = liftM (\d -> [[Plain d]])
                    $ optional whitespace >> (B.toList . mconcat <$> many listInline) <* newline
        multilineDef :: Parser [Char] ParserState [[Block]]
        multilineDef = try $ do
          optional whitespace >> newline
          s <- many1Till anyChar (try (string "=:" >> newline))
          -- this ++ "\n\n" does not look very good
          ds <- parseFromString parseBlocks (s ++ "\n\n")
          return [ds]

-- | This terminates a block such as a paragraph. Because of raw html
-- blocks support, we have to lookAhead for a rawHtmlBlock.
blockBreak :: Parser [Char] ParserState ()
blockBreak = try (newline >> blanklines >> return ()) <|>
             try (optional spaces >> lookAhead rawHtmlBlock >> return ())

-- raw content

-- | A raw Html Block, optionally followed by blanklines
rawHtmlBlock :: Parser [Char] ParserState Block
rawHtmlBlock = try $ do
  (_,b) <- htmlTag isBlockTag
  optional blanklines
  return $ RawBlock (Format "html") b

-- | Raw block of LaTeX content
rawLaTeXBlock' :: Parser [Char] ParserState Block
rawLaTeXBlock' = do
  guardEnabled Ext_raw_tex
  RawBlock (Format "latex") <$> (rawLaTeXBlock <* spaces)


-- | In textile, paragraphs are separated by blank lines.
para :: Parser [Char] ParserState Block
para = do
    a <-  manyTill inline blockBreak
    return $ (Para . normalizeSpaces . B.toList . mconcat) a

-- Tables

-- | A table cell spans until a pipe |
tableCell :: Parser [Char] ParserState TableCell
tableCell = do
  c <- many1 (noneOf "|\n")
  content <- B.toList . mconcat <$> parseFromString (many1 inline) c
  return $ [ Plain $ normalizeSpaces content ]

-- | A table row is made of many table cells
tableRow :: Parser [Char] ParserState [TableCell]
tableRow = try $ ( char '|' *>
  (endBy1 tableCell (optional blankline *> char '|')) <* newline)

-- | Many table rows
tableRows :: Parser [Char] ParserState [[TableCell]]
tableRows = many1 tableRow

-- | Table headers are made of cells separated by a tag "|_."
tableHeaders :: Parser [Char] ParserState [TableCell]
tableHeaders = let separator = (try $ string "|_.") in
  try $ ( separator *> (sepBy1 tableCell separator) <* char '|' <* newline )

-- | A table with an optional header. Current implementation can
-- handle tables with and without header, but will parse cells
-- alignment attributes as content.
table :: Parser [Char] ParserState Block
table = try $ do
  headers <- option [] tableHeaders
  rows <- tableRows
  blanklines
  let nbOfCols = max (length headers) (length $ head rows)
  return $ Table []
    (replicate nbOfCols AlignDefault)
    (replicate nbOfCols 0.0)
    headers
    rows


-- | Blocks like 'p' and 'table' do not need explicit block tag.
-- However, they can be used to set HTML/CSS attributes when needed.
maybeExplicitBlock :: String  -- ^ block tag name
                    -> Parser [Char] ParserState Block -- ^ implicit block
                    -> Parser [Char] ParserState Block
maybeExplicitBlock name blk = try $ do
  optional $ try $ string name >> attributes >> char '.' >>
    optional whitespace >> optional endline
  blk



----------
-- Inlines
----------


-- | Any inline element
inline :: Parser [Char] ParserState B.Inlines
inline = choice inlineParsers <?> "inline"

-- | Inline parsers tried in order
inlineParsers :: [Parser [Char] ParserState B.Inlines]
inlineParsers = [ str
                , inlineMarkup
                , whitespace
                , endline
                , code
                , escapedInline
                , htmlSpan
                , rawHtmlInline
                , rawLaTeXInline'
                , note
                , try $ (char '[' *> inlineMarkup <* char ']')
                , link
                , image
                , mark
                , (B.str . (:[])) <$> characterReference
                , smartPunctuationI inline
                , symbol
                ]

-- | Inline markups
inlineMarkup :: Parser [Char] ParserState B.Inlines
inlineMarkup = choice [ simpleInline (string "??") (B.cite [])
                      , simpleInline (string "**") B.strong
                      , simpleInline (string "__") B.emph
                      , simpleInline (char '*') B.strong
                      , simpleInline (char '_') B.emph
                      , simpleInline (char '+') B.emph  -- approximates underline
                      , simpleInline (char '-' <* notFollowedBy (char '-')) B.strikeout
                      , simpleInline (char '^') B.superscript
                      , simpleInline (char '~') B.subscript
                      ]

-- | Trademark, registered, copyright
mark :: Parser [Char] st B.Inlines
mark = try $ char '(' >> (try tm <|> try reg <|> copy)

reg :: Parser [Char] st B.Inlines
reg = do
  oneOf "Rr"
  char ')'
  return $ B.str "\174"

tm :: Parser [Char] st B.Inlines
tm = do
  oneOf "Tt"
  oneOf "Mm"
  char ')'
  return $ B.str "\8482"

copy :: Parser [Char] st B.Inlines
copy = do
  oneOf "Cc"
  char ')'
  return $ B.str "\169"

note :: Parser [Char] ParserState B.Inlines
note = try $ do
  ref <- (char '[' *> many1 digit <* char ']')
  notes <- stateNotes <$> getState
  case lookup ref notes of
    Nothing   -> fail "note not found"
    Just raw  -> liftM (B.note . B.fromList) $ parseFromString parseBlocks raw

-- | Special chars
markupChars :: [Char]
markupChars = "\\*#_@~-+^|%=[]&"

-- | Break strings on following chars. Space tab and newline break for
--  inlines breaking. Open paren breaks for mark. Quote, dash and dot
--  break for smart punctuation. Punctuation breaks for regular
--  punctuation. Double quote breaks for named links. > and < break
--  for inline html.
stringBreakers :: [Char]
stringBreakers = " \t\n\r.,\"'?!;:<>«»„“”‚‘’()[]"

wordBoundaries :: [Char]
wordBoundaries = markupChars ++ stringBreakers

-- | Parse a hyphened sequence of words
hyphenedWords :: Parser [Char] ParserState String
hyphenedWords = do
  x <- wordChunk
  xs <-  many (try $ char '-' >> wordChunk)
  return $ intercalate "-" (x:xs)

wordChunk :: Parser [Char] ParserState String
wordChunk = try $ do
  hd <- noneOf wordBoundaries
  tl <- many ( (noneOf wordBoundaries) <|>
               try (notFollowedBy' note *> oneOf markupChars
                     <* lookAhead (noneOf wordBoundaries) ) )
  return $ hd:tl

-- | Any string
str :: Parser [Char] ParserState B.Inlines
str = do
  baseStr <- hyphenedWords
  -- RedCloth compliance : if parsed word is uppercase and immediatly
  -- followed by parens, parens content is unconditionally word acronym
  fullStr <- option baseStr $ try $ do
    guard $ all isUpper baseStr
    acro <- enclosed (char '(') (char ')') anyChar
    return $ concat [baseStr, " (", acro, ")"]
  updateLastStrPosI
  return $ B.str fullStr

-- | Textile allows HTML span infos, we discard them
htmlSpan :: Parser [Char] ParserState B.Inlines
htmlSpan = try $ B.str <$> ( char '%' *> attributes *> manyTill anyChar (char '%') )

-- | Some number of space chars
whitespace :: Parser [Char] ParserState B.Inlines
whitespace = many1 spaceChar >> return B.space <?> "whitespace"

-- | In Textile, an isolated endline character is a line break
endline :: Parser [Char] ParserState B.Inlines
endline = try $ do
  newline >> notFollowedBy blankline
  return B.linebreak

rawHtmlInline :: Parser [Char] ParserState B.Inlines
rawHtmlInline = B.rawInline "html" . snd <$> htmlTag isInlineTag

-- | Raw LaTeX Inline
rawLaTeXInline' :: Parser [Char] ParserState B.Inlines
rawLaTeXInline' = try $ do
  guardEnabled Ext_raw_tex
  B.singleton <$> rawLaTeXInline

-- | Textile standard link syntax is "label":target. But we
-- can also have ["label":target].
link :: Parser [Char] ParserState B.Inlines
link = linkB <|> linkNoB

linkNoB :: Parser [Char] ParserState B.Inlines
linkNoB = try $ do
  name <- mconcat <$> surrounded (char '"') (withQuoteContext InDoubleQuote inline)
  char ':'
  let stopChars = "!.,;:"
  url <- manyTill nonspaceChar (lookAhead $ space <|> try (oneOf stopChars >> (space <|> newline)))
  let name' = if B.toList name == [Str "$"] then B.str url else name
  return $ B.link url "" name'

linkB :: Parser [Char] ParserState B.Inlines
linkB = try $ do
  char '['
  name <- mconcat <$> surrounded (char '"') inline
  char ':'
  url <- manyTill nonspaceChar (char ']')
  let name' = if B.toList name == [Str "$"] then B.str url else name
  return $ B.link url "" name'

-- | image embedding
image :: Parser [Char] ParserState B.Inlines
image = try $ do
  char '!' >> notFollowedBy space
  src <- manyTill anyChar (lookAhead $ oneOf "!(")
  alt <- option "" (try $ (char '(' >> manyTill anyChar (char ')')))
  char '!'
  return $ B.image src alt (B.str alt)

escapedInline :: Parser [Char] ParserState B.Inlines
escapedInline = escapedEqs <|> escapedTag

escapedEqs :: Parser [Char] ParserState B.Inlines
escapedEqs = B.str <$> (try $ string "==" *> manyTill anyChar (try $ string "=="))

-- | literal text escaped btw <notextile> tags
escapedTag :: Parser [Char] ParserState B.Inlines
escapedTag = B.str <$>
  (try $ string "<notextile>" *> manyTill anyChar (try $ string "</notextile>"))

-- | Any special symbol defined in wordBoundaries
symbol :: Parser [Char] ParserState B.Inlines
symbol = B.str . singleton <$> (oneOf wordBoundaries <|> oneOf markupChars)

-- | Inline code
code :: Parser [Char] ParserState B.Inlines
code = code1 <|> code2

code1 :: Parser [Char] ParserState B.Inlines
code1 = B.code <$> surrounded (char '@') anyChar

code2 :: Parser [Char] ParserState B.Inlines
code2 = do
  htmlTag (tagOpen (=="tt") null)
  B.code <$> manyTill anyChar (try $ htmlTag $ tagClose (=="tt"))

-- | Html / CSS attributes
attributes :: Parser [Char] ParserState Attr
attributes = (foldl (flip ($)) ("",[],[])) `fmap` many attribute

attribute :: Parser [Char] ParserState (Attr -> Attr)
attribute = classIdAttr <|> styleAttr <|> langAttr

classIdAttr :: Parser [Char] ParserState (Attr -> Attr)
classIdAttr = try $ do -- (class class #id)
  char '('
  ws <- words `fmap` manyTill anyChar (char ')')
  case reverse ws of
       []                      -> return $ \(_,_,keyvals) -> ("",[],keyvals)
       (('#':ident'):classes') -> return $ \(_,_,keyvals) ->
                                             (ident',classes',keyvals)
       classes'                -> return $ \(_,_,keyvals) ->
                                             ("",classes',keyvals)

styleAttr :: Parser [Char] ParserState (Attr -> Attr)
styleAttr = do
  style <- try $ enclosed (char '{') (char '}') anyChar
  return $ \(id',classes,keyvals) -> (id',classes,("style",style):keyvals)

langAttr :: Parser [Char] ParserState (Attr -> Attr)
langAttr = do
  lang <- try $ enclosed (char '[') (char ']') anyChar
  return $ \(id',classes,keyvals) -> (id',classes,("lang",lang):keyvals)

-- | Parses material surrounded by a parser.
surrounded :: Parser [Char] st t   -- ^ surrounding parser
	    -> Parser [Char] st a    -- ^ content parser (to be used repeatedly)
	    -> Parser [Char] st [a]
surrounded border = enclosed (border *> notFollowedBy (oneOf " \t\n\r")) (try border)

-- | Inlines are most of the time of the same form
simpleInline :: Parser [Char] ParserState t           -- ^ surrounding parser
                -> (B.Inlines -> B.Inlines)       -- ^ Inline constructor
                -> Parser [Char] ParserState B.Inlines   -- ^ content parser (to be used repeatedly)
simpleInline border construct = try $ do
  st <- getState
  before <- if stateQuoteContext st == NoQuote then 
                (<>) <$> (whitespace <|> (string "\n" >> return (B.str "\n"))) 
            else return id 
  body <- traceShow (stateQuoteContext st) $ surrounded border inlineWithAttribute 
  return $  before (construct $ mconcat body) 
  where 
    inlineWithAttribute = (try $ optional attributes) >> notFollowedBy (string "\n\n") 
        >> (withQuoteContextI InSingleQuote inline)
    

-- | Create a singleton list
singleton :: a -> [a]
singleton x = [x]

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart :: Parser [tok] ParserState ()
failUnlessSmart = getOption readerSmart >>= guard

smartPunctuationI :: Parser [Char] ParserState B.Inlines
                 -> Parser [Char] ParserState B.Inlines
smartPunctuationI inlineParser = do
  failUnlessSmart
  choice [ quotedI inlineParser, apostropheI, dashI, ellipsesI ]

apostropheI :: Parser [Char] ParserState B.Inlines
apostropheI = (char '\'' <|> char '\8217') >> return (B.str "\x2019")

quotedI :: Parser [Char] ParserState B.Inlines
       -> Parser [Char] ParserState B.Inlines
quotedI inlineParser = doubleQuotedI inlineParser <|> singleQuotedI inlineParser

withQuoteContextI :: QuoteContext
                 -> Parser [tok] ParserState a
                 -> Parser [tok] ParserState a
withQuoteContextI context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuotedI :: Parser [Char] ParserState B.Inlines
             -> Parser [Char] ParserState B.Inlines
singleQuotedI inlineParser = try $ do
  singleQuoteStartI
  withQuoteContextI InSingleQuote $ many1Till inlineParser singleQuoteEndI >>=
    return . B.singleQuoted . mconcat

doubleQuotedI :: Parser [Char] ParserState B.Inlines
             -> Parser [Char] ParserState B.Inlines
doubleQuotedI inlineParser = try $ do
  doubleQuoteStartI
  withQuoteContextI InDoubleQuote $ do
    contents <- manyTill inlineParser doubleQuoteEndI
    return . B.doubleQuoted . mconcat $ contents

failIfInQuoteContext :: QuoteContext -> Parser [tok] ParserState ()
failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

charOrRef :: [Char] -> Parser [Char] st Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

updateLastStrPosI :: Parser [Char] ParserState ()
updateLastStrPosI = getPosition >>= \p ->
  updateState $ \s -> s{ stateLastStrPos = Just p }

singleQuoteStartI :: Parser [Char] ParserState ()
singleQuoteStartI = do
  failIfInQuoteContext InSingleQuote
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  () <$ charOrRef "'\8216\145"

singleQuoteEndI :: Parser [Char] st ()
singleQuoteEndI = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

doubleQuoteStartI :: Parser [Char] ParserState ()
doubleQuoteStartI = do
  failIfInQuoteContext InDoubleQuote
  try $ do charOrRef "\"\8220\147"
           notFollowedBy . satisfy $ flip elem [' ', '\t', '\n']

doubleQuoteEndI :: Parser [Char] st ()
doubleQuoteEndI = do
  charOrRef "\"\8221\148"
  return ()

ellipsesI :: Parser [Char] st B.Inlines
ellipsesI = do
  try (charOrRef "\8230\133") <|> try (string "..." >> return '…')
  return (B.str "\8230")

dashI :: Parser [Char] ParserState B.Inlines
dashI = do
  oldDashes <- getOption readerOldDashes
  if oldDashes
     then emDashOld <|> enDashOld
     else B.str `fmap` (hyphenDash <|> emDash <|> enDash)

-- Two hyphens = en-dash, three = em-dash
hyphenDash :: Parser [Char] st String
hyphenDash = do
  try $ string "--"
  option "\8211" (char '-' >> return "\8212")

emDash :: Parser [Char] st String
emDash = do
  try (charOrRef "\8212\151")
  return "\8212"

enDash :: Parser [Char] st String
enDash = do
  try (charOrRef "\8212\151")
  return "\8211"

enDashOld :: Parser [Char] st B.Inlines
enDashOld = do
  try (charOrRef "\8211\150") <|>
    try (char '-' >> lookAhead (satisfy isDigit) >> return '–')
  return (B.str "\8211")

emDashOld :: Parser [Char] st B.Inlines
emDashOld = do
  try (charOrRef "\8212\151") <|> (try $ string "--" >> optional (char '-') >> return '-')
  return (B.str "\8212")
