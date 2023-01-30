{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Libs where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Char (toLower, isNumber)
import qualified Data.ByteString.Lazy as B

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)

import System.IO
import Data.Text (unpack, pack)

import GHC.Generics

type URL = String

data Poll = Poll
  { pollTitle         :: String,
    pollPoll_options  :: [Option],
    pollPoll_config   :: Config
  }
  deriving (Show, Generic)

instance FromJSON Poll where
  parseJSON = genericParseJSON $ jsonOptions "poll"

instance ToJSON Poll where
  toEncoding = genericToEncoding $ jsonOptions "poll"

data Option = Option
  { optionEnd_time    :: Integer,
    optionStart_time  :: Integer,
    optionType        :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON Option where
  parseJSON = genericParseJSON $ jsonOptions "option"

instance ToJSON Option where
  toEncoding = genericToEncoding $ jsonOptions "option"

data Config = Config {
    configAllow_indeterminate   :: Bool,
    configIs_multiple_choice    :: Bool,
    configIs_private            :: Bool,
    configVote_type             :: String
} deriving (Eq, Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ jsonOptions "config"

instance ToJSON Config where
  toEncoding = genericToEncoding $ jsonOptions "config"

jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions {fieldLabelModifier = lowercaseFirstCharacter . drop prefixLength}
  where prefixLength = length prefix
        lowercaseFirstCharacter (c : rest) = toLower c : rest
        lowercaseFirstCharacter [] = []

createPoll :: IO String
createPoll = do
  linkHandle <- openFile "link" ReadMode
  url <- hGetLine linkHandle
  hClose linkHandle
  maybePoll <- getPoll url
  case maybePoll of
    Right poll -> do
      print $ encode (updatePoll poll)
      val <- sendPollRequest $ updatePoll poll
      case val of
        (String url) -> do
          linkHandle <- openFile "link" WriteMode
          hPutStr linkHandle (unpack url)
          hClose linkHandle
          return $ "https://strawpoll.com" ++ unpack url
        _ -> error "Error"
    Left e -> error e

shiftByAWeek :: Integer -> Integer
shiftByAWeek = (+) 604800

updateTitle :: String -> String
updateTitle s = case span isNumber <$> break isNumber s of
                  (start , (num , end)) -> start ++ show (incrNum num) ++ end
              where incrNum :: String -> Int
                    incrNum = succ . read

updateOption :: Option -> Option
updateOption o = o { optionEnd_time = shiftByAWeek endTime,
                     optionStart_time = shiftByAWeek startTime }
  where endTime = optionEnd_time o
        startTime = optionStart_time o

updatePoll :: Poll -> Poll
updatePoll p = p { pollPoll_options = updateOption <$> options,
                   pollTitle = updateTitle title }
  where options = pollPoll_options p
        title   = pollTitle p

getPoll :: URL -> IO (Either String Poll)
getPoll path = do
  eitherDecode <$> getLastPollContent path

getLastPollContent :: URL -> IO B.ByteString
getLastPollContent path = do
  manager <- newTlsManager
  request <- HTTP.parseRequest ("https://api.strawpoll.com/v3" ++ path)
  HTTP.responseBody <$> HTTP.httpLbs request manager

buildRequest :: String -> Poll -> IO HTTP.Request
buildRequest url body = do
  nakedRequest <- HTTP.parseRequest url
  return (nakedRequest { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ encode body })

sendPollRequest :: Poll -> IO Value
sendPollRequest s = do
  manager <- newTlsManager
  request <- buildRequest "https://api.strawpoll.com/v3/polls" s
  response <- HTTP.httpLbs request manager
  let Just obj = decode (HTTP.responseBody response)
  case lookup "path" (obj :: KeyMap Value) of
    Just val -> return val
    Nothing  -> error "Something bad happened :("
