module Text.Pandoc.Readers.EPUB
  (readEPUB, testRead)
  where

import Text.Pandoc.Definition hiding (Attr)
import Text.Pandoc.Walk 
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Options
import Text.Pandoc.Shared (escapeURI)
import Codec.Archive.Zip
import Text.XML.Light
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as BL
import System.FilePath
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Monoid
import Data.List (isPrefixOf)
import Debug.Trace
import Data.Maybe
import qualified Data.Map as M

testRead :: String -> IO Pandoc
testRead f = readEPUB def <$> BL.readFile f

readEPUB :: ReaderOptions -> BL.ByteString -> Pandoc
readEPUB opts bytes = fromJust $ archiveToEPUB opts $ toArchive bytes

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName "xmlns" _ _) val) = Just ("xmlns", val)
attrToNSPair _ = Nothing

attrToNSPair1 :: Attr -> Maybe (String, String)
attrToNSPair1 (Attr (QName v Nothing (Just "xmlns")) val) = Just (v, val)
attrToNSPair1 _ = Nothing

attrToPair :: Attr -> (String, String)
attrToPair (Attr (QName name _ _) val) = (name, val)


defaultNameSpace :: Maybe String
defaultNameSpace = Just "http://www.idpf.org/2007/opf"

dfName :: String -> QName
dfName s = QName s defaultNameSpace Nothing

emptyName :: String -> QName
emptyName s = QName s Nothing Nothing

archiveToEPUB :: ReaderOptions -> Archive -> Maybe Pandoc
archiveToEPUB os archive = do
  (root, content) <- getManifest archive
--  meta  <- parseMeta content
  items <- parseManifest content
  spine <- parseSpine items content
  let escapedSpine = map (escapeURI . fst) spine
  walk (prependHash escapedSpine) $ 
    mconcat <$> mapM (parseSpineElem root) spine
  where 
    parseSpineElem r (path, mime) = do
      fname <- findEntryByPath (r </> path) archive
      let fileContents = (UTF8.toStringLazy . fromEntry) fname
      let doc = readHtml os fileContents
      return (fixInternalReferences path doc)
    
    
     
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
  

type MIME = String

mimeToReader :: MIME -> (ReaderOptions -> String -> Pandoc)
mimeToReader m = readHtml

type Items = M.Map String (FilePath, MIME)

parseManifest :: Element -> Maybe Items
parseManifest content = do
  let namespaces = mapMaybe attrToNSPair1 (elAttribs content)
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
    parseItemRef e = do
      let linear = maybe True (== "yes") (findAttr (emptyName "linear") e)
      guard linear
      findAttr (emptyName "idref") e


parseMeta :: Element -> Maybe Meta
parseMeta = undefined



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
  mime <- lookup "media-type" as
  manifest <- findEntryByPath root archive
  (,) rootdir <$> (parseXMLDoc . UTF8.toStringLazy . fromEntry) manifest

