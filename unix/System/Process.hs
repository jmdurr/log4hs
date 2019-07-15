module System.Process (getProcessId) where

import           System.Posix.Process (getProcessID)

getProcessId :: IO Int
getProcessId = fromIntegral <$> getProcessID

