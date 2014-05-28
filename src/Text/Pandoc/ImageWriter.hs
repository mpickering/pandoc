{-

Copyright

Writes emdedded images to file and returns a modified AST with the new local paths.

-}


module Text.Pandoc.ImageWriter (convertImages) where

import Text.Pandoc.Definition
import qualified Data.ByteString as BS
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Maybe (fromJust)
import System.FilePath
import Text.Pandoc.MIME (extensionFromMimeType)
import Text.Pandoc.Generic (bottomUpM)


convertB64 :: Inline -> IO Inline
convertB64 (Image alt title (ImageData mime b64)) = do
  let tmpdir = "images"
  let bs  = unByteString64 b64
  let tit = UTF8.toString $ BS.take 10 bs
  let ext = fromJust $ extensionFromMimeType mime
  let fname = tmpdir </> tit <.> ext
  BS.writeFile fname bs
  return $ Image alt title (ImagePath fname)
convertB64 x = return x

convertImages :: Pandoc -> IO Pandoc
convertImages = bottomUpM convertB64



