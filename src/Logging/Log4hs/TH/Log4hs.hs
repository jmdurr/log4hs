{-# LANGUAGE TemplateHaskell #-}
module Logging.Log4hs.TH.Log4hs (logError,logWarn,logInfo,logDebug,logTrace,logErrorShow,logWarnShow,logInfoShow,logDebugShow,logTraceShow) where
import           Data.List.Split     (splitOn)
import           Language.Haskell.TH
import           Logging.Log4hs

modName :: Q [String]
modName = splitOn "." . loc_module <$> location
-- can collect line number too with fst . !!loc_start although it might be off after expansion?

logWithLevel :: Name -> Q Exp
logWithLevel nm = do
    n <- modName
    return $ AppE (AppE (VarE 'Logging.Log4hs.logMsg) (ListE (map (LitE . stringL) n))) (ConE nm)


logShowWithLevel :: Name -> Q Exp
logShowWithLevel nm = do
    n <- modName
    return $ AppE (AppE (VarE 'Logging.Log4hs.logShow) (ListE (map (LitE . stringL) n))) (ConE nm)

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

logErrorShow :: Q Exp
logErrorShow= logShowWithLevel 'ERROR

logWarnShow :: Q Exp
logWarnShow= logShowWithLevel 'WARN

logInfoShow :: Q Exp
logInfoShow= logShowWithLevel 'INFO

logTraceShow :: Q Exp
logTraceShow= logShowWithLevel 'TRACE

logDebugShow :: Q Exp
logDebugShow= logShowWithLevel 'DEBUG
