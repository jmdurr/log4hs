module Logging.Log4hs.Appender.RollingFile (rollingFileAppender,sizeBasedTriggeringPolicy,timeBasedTriggeringPolicy) where

import           Codec.Compression.GZip  (compress)
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)
import           Control.Monad           (void, when)
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy    as BL
import           Data.IORef              (newIORef, readIORef, writeIORef)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Builder  as TB
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import           Logging.Log4hs
import           System.Directory        (createDirectoryIfMissing, removeFile,
                                          renameFile)
import           System.FilePath         (FilePath, dropExtension,
                                          replaceFileName, takeDirectory,
                                          takeExtension)
import           System.IO


rollingFileAppender :: MonadIO m => Bool -> Bool -> FilePath -> String -> Bool -> Layout m -> [m (TriggeringPolicy,TriggeringPolicyReset)] -> m (LogAppender m,LogAppenderFinish m)
rollingFileAppender append buffer file fp flush layout policiesM = do
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  policies <- sequence policiesM
  let md = if append then AppendMode else WriteMode in do
    h <- liftIO $ openFile file md
    liftIO $ hSetBuffering h (if buffer then BlockBuffering (Just 128000) else NoBuffering)
    ioh <- liftIO $ newMVar (h,1)
    return (\m lvl msg args -> do
              t <- layout m lvl msg args
              r <- liftIO $ or <$> mapM (\(p,_) -> p t) policies
              when r $ liftIO $ modifyMVar_ ioh $ \(h',i) -> do
                  hClose h'
                  mapM_ snd policies
                  roll file fp' i md
              liftIO $ withMVar ioh $ \(h',_) -> do
                hPutStr h' (T.unpack t)
                when flush (hFlush h)

          , liftIO $ withMVar ioh $ \(h',_) -> hClose h'
          )
  where fp' = parseOnly filePattern (T.pack fp)


roll :: (MonadFail m, MonadIO m) => FilePath -> Either String (UTCTime -> Int -> String) -> Int -> IOMode -> m (Handle,Int)
roll f (Right nf) i md = do
  t <- liftIO getCurrentTime
  let fp = replaceFileName f (nf t i)
      gz = takeExtension fp == ".gz"
      ngz = (if gz then dropExtension fp else fp)
    in do
      liftIO $ renameFile f ngz
      when gz $ liftIO $ gzip ngz
      h <- liftIO $ openFile f md
      return (h,i+1)
roll _ (Left e) _ _ = fail $ "could not parse file pattern: " ++ e

gzip :: FilePath -> IO ()
gzip s = void $ forkIO $ do
      bs <- BL.readFile s
      BL.writeFile (s ++ ".gz") (compress bs)
      removeFile s

type TriggeringPolicy = T.Text -> IO Bool
type TriggeringPolicyReset = IO ()

timeBasedTriggeringPolicy :: MonadIO m => Int -> m (TriggeringPolicy,TriggeringPolicyReset)
timeBasedTriggeringPolicy intervalS = do
    tref <- liftIO (getCurrentTime >>= newIORef)
    return
      (const $ do
        t' <- getCurrentTime
        oldt <- readIORef tref
        if intervalS <=  truncate (diffUTCTime t' oldt)
          then writeIORef tref t' >> return True
          else return False
      , getCurrentTime >>= writeIORef tref
      )


sizeBasedTriggeringPolicy :: MonadIO m => Int -> FilePath -> m (TriggeringPolicy,TriggeringPolicyReset)
sizeBasedTriggeringPolicy maxChars file = do
  szref <- liftIO (withFile file ReadWriteMode hFileSize >>= newIORef)
  return
    (\t -> do
      sz <- fromIntegral <$> readIORef szref
      if sz + T.length t > maxChars
        then writeIORef szref 0 >> return True
        else writeIORef szref (fromIntegral sz + toInteger (T.length t)) >> return False
    , writeIORef szref 0
    )

data Token = TokenDate T.Text | TokenInc | TokenPerc | TokenText T.Text

date :: Parser Token
date = do
  void $ string (T.pack "%d{")
  s <- manyTill anyChar (char '}')
  return $ TokenDate (T.pack s)

inc :: Parser Token
inc = string (T.pack "%i") >> return TokenInc

perc :: Parser Token
perc = string (T.pack "%%") >> return TokenPerc

text :: Parser Token
text = TokenText <$> takeWhile1 (/= '%')

filePattern :: Parser (UTCTime -> Int -> String)
filePattern = do
  ts <- many1 $ choice [date,inc,perc,text]
  endOfInput
  return (\t i -> LT.unpack $ TB.toLazyText $ convert ts t i)
  where
    convert [] _ _ = TB.fromString ""
    convert (TokenDate ds : ts) t i = TB.fromString (formatTime defaultTimeLocale (T.unpack ds) t) <> convert ts t i
    convert (TokenInc: ts) t i = TB.fromString (show i) <> convert ts t i
    convert (TokenPerc : ts) t i = TB.singleton '%' <> convert ts t i
    convert (TokenText txt : ts) t i = TB.fromText txt <> convert ts t i

