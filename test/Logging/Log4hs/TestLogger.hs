module Logging.Log4hs.TestLogger where

import           Control.Concurrent   (myThreadId)
import qualified Data.Text            as T
import           Data.Time.Clock
import           Logging.Log4hs.Types
import           Test.Hspec.Core.Spec

instance Logger (SpecM a) where
    loggerLevel = return TRACE
    loggerName = return [T.pack "Logging",T.pack "Log4hs", T.pack "TestLogger"]
    log = runIO . print
    loggerTime = return $ read "2019-07-09 15:21:06.366055272 UTC"
    loggerProcessId = return 1234
    loggerThreadId = runIO myThreadId

instance Logger IO where
    loggerLevel = return TRACE
    loggerName = return [T.pack "Logging",T.pack "Log4hs", T.pack "TestLogger"]
    log = print
    loggerTime = return $ read "2019-07-09 15:21:06.366055272 UTC"
    loggerProcessId = return 1234
    loggerThreadId = myThreadId
