{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Readers.Docx
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of Docx type (defined in Text.Pandoc.Readers.Docx.Parse)
to 'Pandoc' document.  -}

{-
Current state of implementation of Docx entities ([x] means
implemented, [-] means partially implemented):

* Blocks

  - [X] Para
  - [X] CodeBlock (styled with `SourceCode`)
  - [X] BlockQuote (styled with `Quote`, `BlockQuote`, or, optionally,
        indented)
  - [X] OrderedList
  - [X] BulletList
  - [X] DefinitionList (styled with adjacent `DefinitionTerm` and `Definition`)
  - [X] Header (styled with `Heading#`)
  - [ ] HorizontalRule
  - [-] Table (column widths and alignments not yet implemented)

* Inlines

  - [X] Str
  - [X] Emph (From italics. `underline` currently read as span. In
        future, it might optionally be emph as well)
  - [X] Strong
  - [X] Strikeout
  - [X] Superscript
  - [X] Subscript
  - [X] SmallCaps
  - [ ] Quoted
  - [ ] Cite
  - [X] Code (styled with `VerbatimChar`)
  - [X] Space
  - [X] LineBreak (these are invisible in Word: entered with Shift-Return)
  - [ ] Math
  - [X] Link (links to an arbitrary bookmark create a span with the target as
        id and "anchor" class)
  - [-] Image (Links to path in archive. Future option for
        data-encoded URI likely.)
  - [X] Note (Footnotes and Endnotes are silently combined.)
-}

module Text.Pandoc.Readers.Docx
       ( readDocx
       ) where

import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (toList, Inlines, Blocks)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.Shared (extractSpaces)
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.UTF8 (toString)
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (delete, isPrefixOf, (\\), intersect)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64 (encode)
import System.FilePath (combine)
import Data.Monoid
import Debug.Trace
import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)

readDocx :: ReaderOptions
         -> BL.ByteString
         -> Pandoc
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Just docx -> Pandoc nullMeta (docxToBlocks opts docx)
    Nothing   -> error $ "couldn't parse docx file"

styles :: [(String, Inlines -> Inlines)]
styles = [
    ("b", B.strong)
  , ("i", B.emph)
  , ("smallCaps", B.smallcaps)
  , ("strike", B.strikeout)
  , ("superscript", B.superscript)
  , ("subscript", B.subscript)]


runStyleToConstructor :: (String, String) -> (Inlines -> Inlines)
runStyleToConstructor ("rStyle", style) =  B.spanWith ("", [style], [])
runStyleToConstructor ("u", fmt) = B.spanWith ("", [], [("underline", fmt)])
runStyleToConstructor (s, _) = maybe id id (lookup s styles)

parStyleToDivAttr :: ParagraphStyle -> (String, [String], [(String, String)])
parStyleToDivAttr pPr = ("",
                          pStyle pPr,
                          case indent pPr of
                            Just n  -> [("indent", (show n))]
                            Nothing -> []
                         )

codeSpans :: [String]
codeSpans = ["VerbatimChar"]

blockQuoteDivs :: [String]
blockQuoteDivs = ["Quote", "BlockQuote"]

codeDivs :: [String]
codeDivs = ["SourceCode"]


runToString :: Run -> String
runToString (Run _ es) = runsToString es
runToString (TextRun s) = s
runToString (LnBrk) = "\n"
runToString _ = ""

runsToString :: [Run] -> String
runsToString = concatMap runToString

runsToInlines :: ReaderOptions -> DocX -> [Run] -> Inlines
runsToInlines o d rs = mconcat $ map (runToInlines o d) rs

runToInlines :: ReaderOptions -> DocX -> Run -> Inlines
runToInlines o d (Run rs runElems) =
  let style = lookup "rStyle" rs in
  case isJust $ (flip elem codeSpans) <$> style of
    True -> B.code (runsToString runElems)
    False ->
      let f = (\s -> foldr runStyleToConstructor s rs) in
      extractSpaces f (runsToInlines o d runElems)
runToInlines opts docx@(DocX _ notes _ _ _ ) (Footnote fnId) =
  case (getFootNote fnId notes) of
    Just bodyParts ->
      B.note $ B.divWith ("", ["footnote"], []) (bodyPartsToBlock opts docx bodyParts)
    Nothing        ->
      B.note $ B.divWith ("", ["footnote"], []) mempty
runToInlines opts docx@(DocX _ notes _ _ _) (Endnote fnId) =
  case (getEndNote fnId notes) of
    Just bodyParts ->
      B.note $ B.divWith ("", ["endnote"], []) (bodyPartsToBlock opts docx bodyParts)
    Nothing        ->
      B.note $ B.divWith ("", ["endnote"], []) mempty
runToInlines _ _ (TextRun s) = B.text s
runToInlines _ _ (LnBrk) = B.linebreak

parPartToInlines :: ReaderOptions -> DocX -> ParPart -> Inlines
parPartToInlines opts docx (PlainRun r) = runsToInlines opts docx [r]
parPartToInlines _ _ (BookMark _ anchor) =
  B.spanWith (anchor, ["anchor"], []) mempty
parPartToInlines _ (DocX _ _ _ rels _) (Drawing relid) =
  case lookupRelationship relid rels of
    Just target -> B.image (combine "word" target) "" mempty
    Nothing     -> B.image "" "" mempty
parPartToInlines opts docx (InternalHyperLink anchor runs) =
  B.link  ('#' : anchor)  "" (runsToInlines opts docx runs)
parPartToInlines opts docx@(DocX _ _ _ rels _) (ExternalHyperLink relid runs) =
  case lookupRelationship relid rels of
    Just target ->
      B.link target "" (runsToInlines opts docx runs)
    Nothing ->
      B.link "" "" (runsToInlines opts docx runs)

isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (ident, classes, kvs) ils) =
  (not . null) ident &&
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchors :: Block -> Block
makeHeaderAnchors h@(Header n (_, classes, kvs) ils) =
  case filter isAnchorSpan ils of
    []   -> h
    (x@(Span (ident, _, _) _) : xs) ->
      case ident `elem` dummyAnchors of
        True -> h
        False -> Header n (ident, classes, kvs) (ils \\ (x:xs))
    _ -> h
makeHeaderAnchors blk = blk

parPartsToInlines :: ReaderOptions -> DocX -> [ParPart] -> Inlines
parPartsToInlines opts docx parparts =
  --
  -- We're going to skip data-uri's for now. It should be an option,
  -- not mandatory.
  --
  --bottomUp (makeImagesSelfContained docx) $
  mconcat $ map (parPartToInlines opts docx) parparts

cellToBlocks :: ReaderOptions -> DocX -> Cell -> Blocks
cellToBlocks opts docx (Cell bps) = mconcat $ map (bodyPartToBlock opts docx) bps

rowToBlocksList :: ReaderOptions -> DocX -> Row -> [Blocks]
rowToBlocksList opts docx (Row cells) = map (cellToBlocks opts docx) cells

bodyPartsToBlock :: ReaderOptions -> DocX -> [BodyPart] -> Blocks
bodyPartsToBlock o d bs = mconcat $ map (bodyPartToBlock o d) bs

bodyPartToBlock :: ReaderOptions -> DocX -> BodyPart -> Blocks
bodyPartToBlock opts docx (Paragraph pPr parparts) =
  B.divWith (parStyleToDivAttr pPr) (B.para $ parPartsToInlines opts docx parparts)
bodyPartToBlock opts docx@(DocX _ _ numbering _ _) (ListItem pPr numId lvl parparts) =
  let
    kvs = case lookupLevel numId lvl numbering of
      Just (_, fmt, txt, Just start) -> [ ("level", lvl)
                                        , ("num-id", numId)
                                        , ("format", fmt)
                                        , ("text", txt)
                                        , ("start", (show start))
                                        ]

      Just (_, fmt, txt, Nothing)    -> [ ("level", lvl)
                                        , ("num-id", numId)
                                        , ("format", fmt)
                                        , ("text", txt)
                                        ]
      Nothing                        -> []
  in
   B.divWith
   ("", ["list-item"], kvs)
   (bodyPartToBlock opts docx (Paragraph pPr parparts))
bodyPartToBlock _ _ (Tbl _ _ _ []) =
  B.para mempty -- Does this do something or can it be replaced with mempty?
bodyPartToBlock opts docx (Tbl cap _ look (r:rs)) =
  let caption = B.text cap
      (hdr, rows) = case firstRowFormatting look of
        True -> (Just r, rs)
        False -> (Nothing, r:rs)
      hdrCells = case hdr of
        Just r' -> rowToBlocksList opts docx r'
        Nothing -> []
      cells = map (rowToBlocksList opts docx) rows

      size = case null hdrCells of
        True -> length $ head cells
        False -> length $ hdrCells
      --
      -- The two following variables (horizontal column alignment and
      -- relative column widths) go to the default at the
      -- moment. Width information is in the TblGrid field of the Tbl,
      -- so should be possible. Alignment might be more difficult,
      -- since there doesn't seem to be a column entity in docx.
      alignments = take size (repeat AlignDefault)
      widths = take size (repeat 0) :: [Double]
  in
   B.table caption (zip alignments widths) hdrCells cells

makeImagesSelfContained :: Docx -> Inline -> Inline
makeImagesSelfContained (Docx _ _ _ _ media) i@(Image alt (uri, title)) =
  case lookup uri media of
    Just bs -> case getMimeType uri of
      Just mime ->  let data_uri =
                          "data:" ++ mime ++ ";base64," ++ toString (encode $ BS.concat $ BL.toChunks bs)
                    in
                     Image alt (data_uri, title)
      Nothing  -> i
    Nothing -> i
makeImagesSelfContained _ inline = inline

bodyToBlocks :: ReaderOptions -> Docx -> Body -> [Block]
bodyToBlocks opts docx (Body bps) =
  bottomUp removeEmptyPars $
  bottomUp spanRemove $
  bottomUp divRemove $
  map (makeHeaderAnchors) $
  bottomUp divCorrect $
  bottomUp divReduce $
  bottomUp divCorrectPreReduce $
  bottomUp blocksToDefinitions $
  blocksToBullets $
  toList $ mconcat $ map (bodyPartToBlock opts docx) bps

docxToBlocks :: ReaderOptions -> Docx -> [Block]
docxToBlocks opts d@(Docx (Document _ body) _ _ _ _) = bodyToBlocks opts d body

ilToCode :: Inline -> String
ilToCode (Str s) = s
ilToCode _ = ""

spanRemove' :: Inline -> [Inline]
spanRemove' s@(Span (ident, classes, _) [])
  -- "_GoBack" is automatically inserted. We don't want to keep it.
  | classes == ["anchor"] && not (ident `elem` dummyAnchors) = [s]
spanRemove' (Span (_, _, kvs) ils) =
  case lookup "underline" kvs of
    Just val -> [Span ("", [], [("underline", val)]) ils]
    Nothing  -> ils
spanRemove' il = [il]

spanRemove :: [Inline] -> [Inline]
spanRemove = concatMap spanRemove'

removeEmptyPars :: [Block] -> [Block]
removeEmptyPars blks = filter (\b -> b /= (Para [])) blks

divReduce :: [Block] -> [Block]
divReduce [] = []
divReduce ((Div (id1, classes1, kvs1) blks1) : blks)
  | (id1, classes1, kvs1) == ("", [], []) = blks1 ++ (divReduce blks)
divReduce (d1@(Div (id1, classes1, kvs1) blks1) :
           d2@(Div (id2, classes2, kvs2) blks2) :
            blks) =
  let classes'  = classes1 `intersect` classes2
      kvs'      = kvs1 `intersect` kvs2
      classes1' = classes1 \\ classes'
      kvs1'     = kvs1 \\ kvs'
      classes2' = classes2 \\ classes'
      kvs2'     = kvs2 \\ kvs'
  in
   case null classes' && null kvs' of
     True -> d1 : (divReduce (d2 : blks))
     False -> let attr'  = ("", classes', kvs')
                  attr1' = (id1, classes1', kvs1')
                  attr2' = (id2, classes2', kvs2')
              in
               divReduce (Div attr' [(Div attr1' blks1), (Div attr2' blks2)] :
                           blks)
divReduce (blk:blks) = blk : (divReduce blks)

isHeaderClass :: String -> Maybe Int
isHeaderClass s | "Heading" `isPrefixOf` s =
  case reads (drop (length "Heading") s) :: [(Int, String)] of
    [] -> Nothing
    ((n, "") : []) -> Just n
    _       -> Nothing
isHeaderClass _ = Nothing

findHeaderClass :: [String] -> Maybe Int
findHeaderClass ss = case mapMaybe id $ map isHeaderClass ss of
  [] -> Nothing
  n : _ -> Just n

blksToInlines :: [Block] -> [Inline]
blksToInlines (Para ils : _) = ils
blksToInlines (Plain ils : _) = ils
blksToInlines _ = []

divCorrectPreReduce' :: Block -> [Block]
divCorrectPreReduce' (Div (ident, classes, kvs) blks)
  | isJust $ findHeaderClass classes =
    let n = fromJust $ findHeaderClass classes
    in
    [Header n (ident, delete ("Heading" ++ (show n)) classes, kvs) (blksToInlines blks)]
  | otherwise = [Div (ident, classes, kvs) blks]
divCorrectPreReduce' blk = [blk]

divCorrectPreReduce :: [Block] -> [Block]
divCorrectPreReduce = concatMap divCorrectPreReduce'

blkToCode :: Block -> String
blkToCode (Para []) = ""
blkToCode (Para ((Code _ s):ils)) = s ++ (blkToCode (Para ils))
blkToCode (Para ((Span (_, classes, _) ils'): ils))
  | (not . null) (codeSpans `intersect` classes) =
    (init $ unlines $ map ilToCode ils') ++ (blkToCode (Para ils))
blkToCode _ = ""

divRemove' :: Block -> [Block]
divRemove' (Div (_, _, kvs) blks) =
  case lookup "indent" kvs of
    Just val -> [Div ("", [], [("indent", val)]) blks]
    Nothing  -> blks
divRemove' blk = [blk]

divRemove :: [Block] -> [Block]
divRemove = concatMap divRemove'

divCorrect' :: Block -> [Block]
divCorrect' b@(Div (ident, classes, kvs) blks)
  | (not . null) (blockQuoteDivs `intersect` classes) =
    [BlockQuote [Div (ident, classes \\ blockQuoteDivs, kvs) blks]]
  | (not . null) (codeDivs `intersect` classes) =
    [CodeBlock (ident, (classes \\ codeDivs), kvs) (init $ unlines $ map blkToCode blks)]
  | otherwise =
      case lookup "indent" kvs of
        Just "0" -> [Div (ident, classes, filter (\kv -> fst kv /= "indent") kvs) blks]
        Just _   ->
          [BlockQuote [Div (ident, classes, filter (\kv -> fst kv /= "indent") kvs) blks]]
        Nothing  -> [b]
divCorrect' blk = [blk]

divCorrect :: [Block] -> [Block]
divCorrect = concatMap divCorrect'
