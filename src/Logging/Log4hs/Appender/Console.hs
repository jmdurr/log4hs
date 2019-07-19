module Logging.Log4hs.Appender.Console (ConsoleAppenderTarget(..),consoleAppender) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (unpack)
import           Logging.Log4hs.Types
import           System.IO              (hPutStr, putStr, stderr, stdout)

data ConsoleAppenderTarget = SYSTEM_OUT | SYSTEM_ERR
consoleAppender :: MonadIO m => ConsoleAppenderTarget -> Layout m -> m (LogAppender m,LogAppenderFinish m)
consoleAppender SYSTEM_OUT lyt = return (\lnm lvl txt args -> lyt lnm lvl txt args >>= liftIO . putStr . unpack, return ())
consoleAppender SYSTEM_ERR lyt = return (\lnm lvl txt args -> lyt lnm lvl txt args >>= liftIO . hPutStr stderr . unpack, return ())
