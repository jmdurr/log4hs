module Logging.Log4hs.TestLogger where

import           Control.Concurrent              (myThreadId)
import qualified Data.Text                       as T
import           Data.Time.Clock
import           Logging.Log4hs
import           Logging.Log4hs.Appender.Console
import           Logging.Log4hs.Layout.Pattern
import           Test.Hspec.Core.Spec


testContext :: LogContext IO
testContext = LogContext {ctxTime = return $ read "2019-07-09 15:21:06.366055272 UTC"
                         ,ctxProcessId = return 1234
                         ,ctxThreadId = myThreadId
                         ,ctxAppenders = [("console",consoleAppender SYSTEM_OUT (patternLayout (T.pack "%maxLen{%d{%T%Q}}{13} [%T] %level %logger{36} - %msg%n")))]
                         ,ctxConfigs = LogConfig { configLevel = TRACE
                                                 , configAppenders = ["console"]
                                                 , configChildren = []
                                                 , configName = ""

                         }
}
