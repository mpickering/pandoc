{-# LANGUAGE OverloadedStrings, CPP #-}
{-
Copyright (C) 2012-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.PDF
   Copyright   : Copyright (C) 2012-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( makePDF ) where

import System.IO.Temp
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BS
import System.Exit (ExitCode (..))
import System.FilePath
import System.Directory
import System.Environment
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.ByteString.Base64 as B64
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Shared (fetchItem, warn)
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.MIME (extensionFromMimeType)
import Text.Pandoc.Process (pipeProcess)
import qualified Data.ByteString.Lazy as BL
#ifdef _WINDOWS
import Data.List (intercalate)
#endif

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir =
#ifdef _WINDOWS
  withTempDirectory "."
#else
  withSystemTempDirectory
#endif

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators = intercalate "/" . splitDirectories
#endif

makePDF :: String              -- ^ pdf creator (pdflatex, lualatex, xelatex)
        -> (WriterOptions -> Pandoc -> String)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> IO (Either ByteString ByteString)
makePDF program writer opts doc = withTempDir "tex2pdf." $ \tmpdir -> do
  doc' <- handleImages (writerSourceURL opts) tmpdir doc
  let source = writer opts doc'
  tex2pdf' tmpdir program source

handleImages :: Maybe String  -- ^ source base URL
             -> FilePath      -- ^ temp dir to store images
             -> Pandoc        -- ^ document
             -> IO Pandoc
handleImages baseURL tmpdir = walkM (handleImage' baseURL tmpdir)

handleImage' :: Maybe String
             -> FilePath
             -> Inline
             -> IO Inline
handleImage' baseURL tmpdir (Image ils tit t) = do
    case t of
      ImagePath src -> do
        exists <- doesFileExist src
        if exists
          then return $ Image ils tit (ImagePath src)
          else do
            res <- fetchItem baseURL src
            case res of
                  Right (contents, Just mime) -> do
                    let ext = fromMaybe (takeExtension src) $
                             extensionFromMimeType mime
                    let basename = UTF8.toString $ B64.encode $ UTF8.fromString src
                    let fname = tmpdir </> basename <.> ext
                    BS.writeFile fname contents
                    return $ Image ils tit (ImagePath fname)
                  _ -> do
                    warn $ "Could not find image `" ++ src ++ "', skipping..."
                    return $ Image ils tit (ImagePath src)
      ImageData mime b64 -> do
        let bs  = unByteString64 b64
        let basename = UTF8.toString $ BS.take 10 bs
        let ext = fromJust $ extensionFromMimeType mime
        let fname = tmpdir </> basename <.> ext
        BS.writeFile fname bs
        return $ Image ils tit (ImagePath fname)
handleImage' _ _ x = return x

tex2pdf' :: FilePath                        -- ^ temp directory for output
         -> String                          -- ^ tex program
         -> String                          -- ^ tex source
         -> IO (Either ByteString ByteString)
tex2pdf' tmpDir program source = do
  let numruns = if "\\tableofcontents" `isInfixOf` source
                   then 3  -- to get page numbers
                   else 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- runTeXProgram program numruns tmpDir source
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> do
          let logmsg = extractMsg log'
          let extramsg =
                case logmsg of
                     x | "! Package inputenc Error" `BC.isPrefixOf` x ->
                           "\nTry running pandoc with --latex-engine=xelatex."
                     _ -> ""
          return $ Left $ logmsg <> extramsg
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> return $ Right pdf

(<>) :: ByteString -> ByteString -> ByteString
(<>) = B.append

-- parsing output

extractMsg :: ByteString -> ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)

-- running tex programs

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
runTeXProgram :: String -> Int -> FilePath -> String
              -> IO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram program runsLeft tmpDir source = do
    let file = tmpDir </> "input.tex"
    exists <- doesFileExist file
    unless exists $ UTF8.writeFile file source
#ifdef _WINDOWS
    -- note:  we want / even on Windows, for TexLive
    let tmpDir' = changePathSeparators tmpDir
    let file' = changePathSeparators file
#else
    let tmpDir' = tmpDir
    let file' = file
#endif
    let programArgs = ["-halt-on-error", "-interaction", "nonstopmode",
         "-output-directory", tmpDir', file']
    env' <- getEnvironment
    let sep = searchPathSeparator:[]
    let texinputs = maybe (tmpDir' ++ sep) ((tmpDir' ++ sep) ++)
          $ lookup "TEXINPUTS" env'
    let env'' = ("TEXINPUTS", texinputs) :
                  [(k,v) | (k,v) <- env', k /= "TEXINPUTS"]
    (exit, out, err) <- pipeProcess (Just env'') program programArgs BL.empty
    if runsLeft > 1
       then runTeXProgram program (runsLeft - 1) tmpDir source
       else do
         let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
         pdfExists <- doesFileExist pdfFile
         pdf <- if pdfExists
                   -- We read PDF as a strict bytestring to make sure that the
                   -- temp directory is removed on Windows.
                   -- See https://github.com/jgm/pandoc/issues/1192.
                   then (Just . B.fromChunks . (:[])) `fmap` BS.readFile pdfFile
                   else return Nothing
         return (exit, out <> err, pdf)

