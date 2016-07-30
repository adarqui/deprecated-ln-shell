module Shell.Editor (
  maybeEditor
) where

import           Control.Exception      (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.Cmd             (rawSystem)
import           System.Directory       (removeFile)
import           System.Environment     (getEnv, lookupEnv)
import           System.IO              (hClose)
import           System.IO.Temp         (openTempFile, withSystemTempFile)



maybeEditor :: (Monad m, MonadIO m) => String -> m Text
maybeEditor "$" = do
--  withSystemTempFile "leuron.lntmp" (\file handle -> liftIO $ print file)
  liftIO $ bracket
    (openTempFile "/tmp" "leuron.lntmp")
    (\(path, h) -> removeFile path)
    (\(path, h) -> do
      hClose h
      code <- rawSystem "/usr/bin/vi" [path]
      contents <- readFile path
      return $ T.pack contents

      {-
       - FIXME: editor style
      editor' <- lookupEnv "EDITOR"
      print editor'
      case editor' of
        Nothing -> return ""
        Just editor -> do
          rawSystem editor [path]
          contents <- readFile path
          return $ T.pack contents
          -}
    )
maybeEditor  s = return $ T.pack s
