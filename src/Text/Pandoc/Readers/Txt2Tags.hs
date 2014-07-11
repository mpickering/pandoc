module Text.Pandoc.Readers.Txt2Tags where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder ( Inlines, Blocks, HasMeta(..), (<>)
                                     , trimInlines )
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.LaTeX (inlineCommand, rawLaTeXInline)
import           Text.Pandoc.Shared (escapeURI,compactify', compactify'DL)
import           Text.TeXMath (texMathToPandoc, DisplayType(..))
import Text.Pandoc.Parsing

import           Control.Applicative ( Applicative, pure
                                     , (<$>), (<$), (<*>), (<*), (*>), (<**>) )
import           Control.Monad (foldM, guard, liftM, liftM2, mplus, mzero, when, void)
import           Data.Char (isAlphaNum, toLower)
import           Data.Default
import           Data.List (intersperse, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isJust, maybe)
import           Data.Monoid (Monoid, mconcat, mempty, mappend)
import           Network.HTTP (urlEncode)
import Debug.Trace
import Data.Sequence (viewr, ViewR(..))
import Network.URI (isURI)
import Control.Monad.Reader
import Text.Parsec.Prim

validChars :: String
validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-"

data T2TState = T2TState { indent :: Int }

instance Default T2TState where
  def = T2TState 0 

type T2T = ParserT String ParserState (Reader T2TState)


parseTxt2Tags :: ReaderOptions -> String -> Pandoc
parseTxt2Tags opts s = either (error . show) id (flip runReader def (runParserT parseT2T (def {stateOptions = opts}) "" s))


parseT2T :: T2T Pandoc
parseT2T = do
  meta <- (Nothing <$ return blankline) <|> (Just <$> return (count 3 anyLine))
  config <- manyTill setting (notFollowedBy setting)
  body <- mconcat <$>  many block
  return eof
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

block :: T2T Blocks
block = choice 
  [ title
  , quote 0 
--  , verbatim
--  , hrule
--  , strongRule
--  , table
--  , para
--  , blockQuote
--  , list
--  , raw
--  , tagged ] 
  ]

title :: T2T Blocks
title = try $ balancedTitle '+' <|> balancedTitle '='
 

balancedTitle :: Char -> T2T Blocks
balancedTitle c = try $ do
  level <- length <$> many1 (char c) 
  traceShow level (return ())
  guard (level <= 5) -- Max header level 5
  heading <- manyTill anyChar (lookAhead $ char c)
  count level (char c)
  label <- optionMaybe (enclosed (char '[') (char ']') (oneOf validChars))
  spaces *> newline
  let attr = maybe nullAttr (\x -> (x, [], [])) label
  return $ B.headerWith attr level (B.str heading)

para :: T2T Blocks
para = B.para <$> trimInlines . mconcat <$> many1 inline

commentBlock :: T2T Blocks 
commentBlock = blockMarkup (return <$> anyLine) (const mempty) "%%%"

verbatim :: T2T Blocks 
verbatim = blockMarkup anyLine B.codeBlock "```" <|>
            try (B.codeBlock <$> (string "``` " <* anyLine))

-- Seperator and Strong line treated the same
hrule :: T2T Blocks 
hrule = try $ do
  spaces 
  line <- manyTill (oneOf "=-_") (newline <|> space)
  guard (length line >= 20)
  B.horizontalRule <$ blankline 
  
  
quote :: Int -> T2T Blocks
quote indent = do
  B.blockQuote . B.para . mconcat <$> 
    many1 (count (indent + 1) tab *> inline <* newline)
  
  


blockMarkup :: Monoid a => (T2T a) -> (a -> Blocks) -> String -> T2T Blocks
blockMarkup p f s = return . try $ do
  string s *> blankline
  return . f . mconcat <$> (manyTill p (string s *> blankline))

  
-- Inline

inline :: T2T Inlines
inline = choice 
  [ comment
  , bold
  , underline
  , strike
  , italic
  , code
  , str
  , symbol
  , whitespace
  ]

comment :: T2T Inlines
comment = try $ mempty <$ (char '%' *> anyLine)

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
raw = inlineMarkup ((:[]) <$> anyChar) B.code '"'

tagged :: T2T Inlines
tagged = inlineMarkup ((:[]) <$> anyChar) B.code '\''

inlineMarkup :: Monoid a => (T2T a) -> (a -> Inlines) -> Char -> T2T Inlines
inlineMarkup p f c = try $ do
  start <- many1 (char c)
  guard (length start >= 2)
  lookAhead (noneOf [' ', c])
  rawBody <-  manyTill anyChar (try $ lookAhead (noneOf " " >> string [c,c] ))
  lastChar <- anyChar 
  end <- many1 (char c)
  s <- getState
  let parser inp = runParser (mconcat <$> many p) s "" inp 
  let start' = either (const mempty) id (parser (drop 2 start))
  let Right body = parser (rawBody ++ [lastChar])
  let end' = either (const mempty) id (parser (drop 2 start))
  return $ f (start' <> body <> end')

link :: T2T Inlines
link = do
  head <- char '[' *> (try image <|> (B.text <$> manyTill (anyChar) (lookAhead (many1 $ noneOf " " >> char ']')))) 
  link <- manyTill anyChar (char ']') 
  return $ B.link (escapeURI link) "" head
   

image :: T2T Inlines
image = return $ (\url -> B.image url "" mempty) <$> enclosed (char '[') (char ']') (noneOf " ")


          

-- Characters used in markup
specialChars :: String
specialChars = "%*-_/"

tab = char '\t'

str :: T2T Inlines 
str = try $ B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")

whitespace :: T2T Inlines 
whitespace = try $ B.space <$ (many1 (oneOf "\n\r "))

symbol :: T2T Inlines
symbol = B.str . (:[]) <$> oneOf specialChars 

-- Fix Up

--isURI

-- Utility

ignoreSpacesCap :: T2T String -> T2T String
ignoreSpacesCap p = map toLower <$> (spaces *> p <* spaces)

