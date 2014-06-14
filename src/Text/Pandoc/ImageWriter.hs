{-

Copyright

Writes emdedded images to file and returns a modified AST with the new local paths.

-}


module Text.Pandoc.ImageWriter (convertImages) where

import Text.Pandoc.Definition
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import System.FilePath
import Text.Pandoc.MIME (extensionFromMimeType)
import Text.Pandoc.Generic (bottomUpM)
import Data.Digest.Pure.SHA (showDigest, sha1)
import System.Directory (doesFileExist)
import Control.Monad (unless)



convertB64 :: Inline -> IO Inline
convertB64 (Image alt title (ImageData mime b64)) = do
  let tmpdir = "images"
  let bs  = either error id  (B64.decode $ unByteString64 b64)
  let tit = showDigest $ sha1 $ BL.fromStrict bs
  let ext = fromJust $ extensionFromMimeType mime
  let fname = tmpdir </> tit <.> ext
  exists <- doesFileExist fname
  unless exists $ BS.writeFile fname bs
  return $ Image alt title (ImagePath fname)
convertB64 x = return x

convertImages :: Pandoc -> IO Pandoc
convertImages = bottomUpM convertB64
