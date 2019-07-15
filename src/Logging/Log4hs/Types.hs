module Logging.Log4hs.Types where
import           Control.Applicative       ((<|>))
import           Control.Concurrent        (ThreadId)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Foldable             (asum)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text, pack, unpack)
import           Data.Time.Clock           (UTCTime)
import           System.IO                 (hPutStr, stderr, stdout)

data LogLevel = ERROR | WARN | INFO | DEBUG | TRACE deriving (Show,Eq,Read)

type Layout m = [String] -> LogLevel -> Text -> [(Text,Text)] -> Logger m Text

type LogAppender m = [String] -> LogLevel -> Text -> [(Text,Text)] -> Logger m ()

type LogAppenderFinish m = m ()


data LogContext m = LogContext { ctxAppenders :: [(String,LogAppender m,LogAppenderFinish m)]
                               , ctxConfigs   :: LogConfig
                               , ctxTime      :: m UTCTime
                               , ctxProcessId :: m Int
                               , ctxThreadId  :: m ThreadId
                               }

--data ReliabilityStrategy = ReliabilityStrategy {}
data LogConfig = LogConfig { configLevel     :: LogLevel
                           , configAppenders :: [String]
                           --, configReliabilityStrategy :: ReliabilityStrategy
                           , configChildren  :: [LogConfig]
                           , configName      :: String
                           }

type Logger m a = ReaderT (LogContext m) m a




