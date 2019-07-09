module Logging.Log4hs.Types where
import           Control.Concurrent (ThreadId)
import           Data.Text
import           Data.Time.Clock    (UTCTime)
data LogLevel = ERROR | WARN | INFO | DEBUG | TRACE deriving (Show,Eq,Read)

type Layout m = [String] -> LogLevel -> Text -> [(Text,Text)] -> m Text


class Monad m => Logger m where
    loggerLevel :: m LogLevel
    log :: [String] -> LogLevel -> Text -> [(Text,Text)] -> m ()
    loggerTime :: m UTCTime
    loggerProcessId :: m Int
    loggerThreadId :: m ThreadId





