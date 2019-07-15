module Logging.Log4hs.Appender.File (fileAppender) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class
import           Data.Text              (unpack)
import           Logging.Log4hs
import           System.IO



fileAppender :: MonadIO m => String -> Bool -> Bool -> FilePath -> Bool -> Layout m -> m (String,LogAppender m,LogAppenderFinish m)
fileAppender nm append buffer file flush layout = do
    h <- liftIO $ do
        h' <- openFile file (if append then AppendMode else WriteMode)
        unless buffer $ hSetBuffering h' NoBuffering
        return h'
    return (nm
           ,\m lvl msg args -> do
                txt <- layout m  lvl msg args
                liftIO $ hPutStr h (unpack txt)
           ,liftIO $ hClose h
           )
