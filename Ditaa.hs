module Ditaa (plugin) where

-- This plugin allows you to include a [ditaa diagram](http://ditaa.sourceforge.net)
-- in a page like this:
--
-- ~~~ {.ditaa}
-- +--------+   +-------+    +-------+
-- |        | --+ ditaa +--> |       |
-- |  Text  |   +-------+    |diagram|
-- |Document|   |!magic!|    |       |
-- |     {d}|   |       |    |       |
-- +---+----+   +-------+    +-------+
--     :                         ^
--     |       Lots of work      |
--     +-------------------------+
-- ~~~
--
-- The `java` executable must be in the path, and `ditaaXXX.jar` must be in the
-- current directory.
--
-- You can get the latest .jar from [here](http://ditaa.sourceforge.net/#download)
--
-- The generated PNG file will be saved in the static `img` directory, using a unique
-- name generated from a hash of the contents.

import Network.Gitit.Interface

import Control.Exception
import Control.Monad
import Control.Monad.Trans (liftIO)

import Data.List
import Data.Maybe

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

-- utf8-string package
import Data.ByteString.Lazy.UTF8 (fromString)
-- SHA package
import Data.Digest.Pure.SHA

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "ditaa" `elem` classes = do
    cfg <- askConfig
    let outfile_web = "img" </> uniqueName contents <.> "png"
        outfile = staticDir cfg </> outfile_web
    
    liftIO $ unlessM (doesFileExist outfile) $ do
        -- 0) Establish a temporary file name to store the data into before running ditaa
        tmp_dir <- getTemporaryDirectory
        withTempFile tmp_dir "ditaa.txt" $ \temp_file temp_file_h -> do
            -- 1) Setup input file by writing contents to it:
            hPutStrLn temp_file_h contents
            hClose temp_file_h
      
            -- 2) Run ditaa to turn into an equivalently named .png:
            ditaa_jar <- findDitaaJar
            let options = ["-jar", ditaa_jar,
                           "-e", "utf8", -- UTF8 input (I think!)
                           "-o"          -- Overwrite existing file if present (shouldn't be necessary)
                          ] ++
                          ["-E" | "no-separation" `elem` classes] ++
                          ["-S" | "no-shadows" `elem` classes] ++
                          [temp_file]
            (ec, _, stderr_out) <- readProcessWithExitCode "java" options ""
            if ec == ExitSuccess
               then copyFile (replaceExtension temp_file ".png") outfile
               else error $ "Error running ditaa: " ++ stderr_out
    
    return $ Para [Image [] ("/" ++ outfile_web, "")]
transformBlock x = return x

-- | Find the ditaaXXX.jar file in the current directory
findDitaaJar :: IO FilePath
findDitaaJar = fmap (fromMaybe (error "Could not locate the ditaa .jar file in the current directory") . maybeHead .
                     filter ("ditaa" `isInfixOf`) . filter (".jar" `isSuffixOf`)) $
                    getCurrentDirectory >>= getDirectoryContents

withTempFile :: FilePath -- ^ Temporary directory to create the file in
             -> String   -- ^ File name template: see 'openTempFile'
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  bracket (openTempFile tmpDir template)
          (\(name, handle) -> hClose handle >> removeFile name)
          (uncurry action)

maybeHead :: [a] -> Maybe a
maybeHead = foldr ((Just .) . const) Nothing

unlessM mb mact = mb >>= \b -> unless b mact

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
