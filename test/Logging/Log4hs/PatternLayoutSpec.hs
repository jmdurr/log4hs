module Logging.Log4hs.PatternLayoutSpec (spec) where

import           Data.Either
import qualified Data.Text                    as T
import           Logging.Log4hs.PatternLayout
import           Logging.Log4hs.TestLogger
import           Logging.Log4hs.Types
import           Test.Hspec

-- type Layout m = LogLevel -> Text -> [(Text,Text)] -> m Text

runLayout :: Monad m => Either String (Layout m) -> LogLevel -> T.Text -> [(T.Text,T.Text)] -> m T.Text
runLayout (Left e) _ _ _         = return $ T.pack e
runLayout (Right l) lvl msg args = l lvl msg args

plainTextLog :: String -> String -> IO ()
plainTextLog pat mat = do
    l <- patternLayout (T.pack pat)
    r <- runLayout l DEBUG (T.pack "some text") []
    r `shouldBe` T.pack mat

fullLog :: String -> String -> [(String,String)] -> String -> IO ()
fullLog pat msg args mat = do
    l <- patternLayout (T.pack pat)
    r <- runLayout l DEBUG (T.pack msg) $ map (\(k,v) -> (T.pack k, T.pack v)) args
    r `shouldBe` T.pack mat


spec :: Spec
spec = do
    describe "parselayout" $ do
        it "should parse empty" $ plainTextLog "" ""
        it "should parse text" $ plainTextLog "hello world" "hello world"
        it "should have process id" $ plainTextLog "%processId%pidhello" "12341234hello"
        it "should have logger name" $ plainTextLog "%cOther" "Logging.Log4hs.TestLoggerOther"
        it "should have short logger name to right" $ plainTextLog "%c{1}" "TestLogger"
        it "should have short logger name to left" $ plainTextLog "%c{-1}" "Log4hs.TestLogger"
        it "should have a date" $ plainTextLog "[%d{%Y-%m-%d}]" "[2019-07-09]"
        it "should encode newline" $ plainTextLog "%encode{CRLF}{hi\nyou\r}" "hi\\nyou\\r"
        it "should encode json" $ plainTextLog "%encode{JSON}{{json:'test',\nobj:\"boo\"}}" "{json:'test',\\u0010obj:\\\"boo\\\"}"
        it "should truncate safely" $ plainTextLog "%maxLen{%encode{CRLF}{hi\n}}{3}" "hi"
        it "should output newline" $ plainTextLog "hi%nyou" "hi\nyou"
        it "should output log level" $ plainTextLog "%p - msg" "DEBUG - msg"
        it "should transform log level" $ plainTextLog "%p{DEBUG=*} - msg" "* - msg"
        it "should add kv" $ fullLog "Hello %K{place}" "msg" [("place","world"),("other","key")] "Hello world"




