module System.Process (getProcessId) where

import           System.Win32.Process (getCurrentProcessId)

getProcessId :: IO Int
getProcessId = fromIntegral <$> getCurrentProcessId
