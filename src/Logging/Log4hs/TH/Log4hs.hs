{-# LANGUAGE TemplateHaskell #-}
module Logging.Log4hs.TH.Log4hs (logError,logWarn,logInfo,logDebug,logTrace) where
import           Data.List.Split            (splitOn)
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Logging.Log4hs

modName :: Q [String]
modName = splitOn "." . loc_module <$> location
-- can collect line number too with fst . !!loc_start although it might be off after expansion?

logWithLevel :: Name -> Q Exp
logWithLevel nm = do
    n <- modName
    return $ AppE (AppE (VarE 'Logging.Log4hs.logMsg) (ListE (map (LitE . stringL) n))) (ConE nm)


logError :: Q Exp
logError = logWithLevel 'ERROR

logWarn :: Q Exp
logWarn = logWithLevel 'WARN

logInfo :: Q Exp
logInfo = logWithLevel 'INFO

logDebug :: Q Exp
logDebug = logWithLevel 'DEBUG

logTrace :: Q Exp
logTrace = logWithLevel 'TRACE

