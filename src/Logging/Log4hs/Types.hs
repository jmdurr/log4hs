module Logging.Log4hs.Types where
import           Control.Concurrent (ThreadId)
import           Data.Text
import           Data.Time.Clock    (UTCTime)
data LogLevel = ERROR | WARN | INFO | DEBUG | TRACE deriving (Show,Eq,Read)

type Layout m = LogLevel -> Text -> [(Text,Text)] -> m Text


class Monad m => Logger m where
    loggerLevel :: m LogLevel
    loggerName :: m [Text]
    log :: Text -> m ()
    loggerTime :: m UTCTime
    loggerProcessId :: m Int
    loggerThreadId :: m ThreadId




