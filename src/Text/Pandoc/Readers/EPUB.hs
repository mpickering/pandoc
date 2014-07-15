{-# LANGUAGE ViewPatterns, BangPatterns, StandaloneDeriving  #-}

module Text.Pandoc.Readers.EPUB
  (readEPUB)
  where

import Text.XML.Light 
import Text.Pandoc.Definition hiding (Attr)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Generic(bottomUp)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Options (ReaderOptions(..), readerExtensions, Extension(..) )
import Text.Pandoc.Shared (escapeURI)
import qualified Text.Pandoc.Builder as B
import Codec.Archive.Zip (Archive (..), toArchive, fromEntry, findEntryByPath) 
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as BL
import System.FilePath
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Monoid (mempty, (<>))
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M (Map,  lookup, fromList)
import qualified Data.Set as S (insert)
import Control.DeepSeq.Generics 

type MIME = String

type Items = M.Map String (FilePath, MIME)

readEPUB :: ReaderOptions -> BL.ByteString -> Pandoc
readEPUB opts bytes = fromJust $ archiveToEPUB opts $ toArchive bytes

archiveToEPUB :: ReaderOptions -> Archive -> Maybe Pandoc
archiveToEPUB os archive = do
  (root, content) <- getManifest archive
  meta  <- parseMeta content
  items <- parseManifest content
  spine <- parseSpine items content
  let escapedSpine = map (escapeURI . fst) spine
  Pandoc _ bs <- 
      foldM' (\a b -> ((a <>) . bottomUp (prependHash escapedSpine)) 
        <$> parseSpineElem root b) mempty spine
  return (Pandoc meta bs)
  where 
    rs = readerExtensions os
    os' = os {readerExtensions = S.insert Ext_epub_html_exts rs}
    parseSpineElem r (path, mime) = do
      fname <- findEntryByPath (r </> path) archive
      let doc = mimeToReader mime fname
      return (fixInternalReferences path doc)
    mimeToReader s = 
      case s of 
        "application/xhtml+xml" -> readHtml os' . UTF8.toStringLazy . fromEntry
        "image/gif" -> return mempty 
        "image/jpeg" -> return mempty
        "image/png" -> return mempty
        _ -> const mempty
    spineImage = B.doc . B.para . (\x -> B.image x "" mempty)

parseManifest :: Element -> Maybe Items
parseManifest content = do
  manifest <- findElement (dfName "manifest") content
  let items = findChildren (dfName "item") manifest
  r <- mapM parseItem items
  return (M.fromList r)

  where
    parseItem e = do
      uid <- findAttr (emptyName "id") e
      href <- findAttr (emptyName "href") e
      mime <- findAttr (emptyName "media-type") e
      return (uid, (href, mime))

parseSpine :: Items -> Element -> Maybe [(FilePath, MIME)]
parseSpine is e = do
  spine <- findElement (dfName "spine") e
  let itemRefs = findChildren (dfName "itemref") spine
  mapM (flip M.lookup is)  $ mapMaybe parseItemRef itemRefs
  where
    parseItemRef ref = do
      let linear = maybe True (== "yes") (findAttr (emptyName "linear") ref)
      guard linear
      findAttr (emptyName "idref") ref

parseMeta :: Element -> Maybe Meta
parseMeta content = do 
  meta <- findElement (dfName "metadata") content
  let dcspace (QName _ (Just "http://purl.org/dc/elements/1.1/") (Just "dc")) = True
      dcspace _ = False
  let dcs = filterChildrenName dcspace meta
  let r = foldr parseMetaItem nullMeta dcs 
  return r

-- http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-metadata-elem
parseMetaItem :: Element -> Meta -> Meta
parseMetaItem e@(stripNamespace . elName -> field) meta = 
  B.setMeta (renameMeta field) (B.str $ strContent e) meta
  
renameMeta :: String -> String
renameMeta "creator" = "author"
renameMeta s = s

getManifest :: Archive -> Maybe (String, Element)
getManifest archive = do
  metaEntry <- findEntryByPath ("META-INF" </> "container.xml") archive
  docElem <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) metaEntry
  let namespaces = mapMaybe attrToNSPair (elAttribs docElem)
  ns <- lookup "xmlns" namespaces
  as <- (map attrToPair) . elAttribs <$>
    findElement (QName "rootfile" (Just ns) Nothing) docElem
  root <- lookup "full-path" as
  let rootdir = dropFileName root
  --mime <- lookup "media-type" as
  manifest <- findEntryByPath root archive
  (,) rootdir <$> (parseXMLDoc . UTF8.toStringLazy . fromEntry) manifest

-- Fixup

fixInternalReferences :: String -> Pandoc -> Pandoc
fixInternalReferences s = 
  (walk $ fixBlockIRs s') . (walk $ fixInlineIRs s')
  where 
    s' = escapeURI s
  
fixInlineIRs :: String -> Inline -> Inline
fixInlineIRs s (Span (ident, cs, kvs) v ) =
  Span (s ++ "#" ++ ident, cs, kvs) v
fixInlineIRs s (Code (ident, cs, kvs) code) = 
  Code (s ++ "#" ++ ident, cs, kvs) code
fixInlineIRs _ v = v

prependHash :: [String] -> Inline -> Inline
prependHash ps l@(Link is (url, tit))
  | or [s `isPrefixOf` url | s <- ps] = 
    Link is ('#':url, tit)
  | otherwise = l
prependHash _ i = i

fixBlockIRs :: String -> Block -> Block
fixBlockIRs s (Div (ident, cs, kvs) b) =
  Div ( s ++ "#" ++ ident , cs, kvs) b
fixBlockIRs s (Header i (ident, cs, kvs) b) = 
  Header i (s ++ "#" ++ ident, cs, kvs) b 
fixBlockIRs s (CodeBlock (ident, cs, kvs) code) = 
  CodeBlock (s ++ "#" ++ ident, cs, kvs) code
fixBlockIRs _ b = b

-- Utility

-- Strict version of foldM
foldM' :: (Monad m, NFData a) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `deepseq` foldM' f z' xs

stripNamespace :: QName -> String
stripNamespace (QName v _ _) = v

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName "xmlns" _ _) val) = Just ("xmlns", val)
attrToNSPair _ = Nothing

--attrToNSPair1 :: Attr -> Maybe (String, String)
--attrToNSPair1 (Attr (QName v Nothing (Just "xmlns")) val) = Just (v, val)
--attrToNSPair1 _ = Nothing

attrToPair :: Attr -> (String, String)
attrToPair (Attr (QName name _ _) val) = (name, val)

defaultNameSpace :: Maybe String
defaultNameSpace = Just "http://www.idpf.org/2007/opf"

dfName :: String -> QName
dfName s = QName s defaultNameSpace Nothing

emptyName :: String -> QName
emptyName s = QName s Nothing Nothing
