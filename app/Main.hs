{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (isPrefixOf, toLower, Text, pack, unpack)
import qualified Data.Text.IO as TIO

import GHC.Generics
import System.Environment (getEnv)

import Control.Monad.Reader

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import System.Cron
import System.Cron.Schedule

import Libs

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> do
        when (fromBot m) (void $ restCall (R.AddPinnedMessage (messageChannelId m, messageId m)))
        when (isPing m && not (fromBot m)) announcePoll
    _ -> return ()

startHandler :: DiscordHandler ()
startHandler = do
    h <- ask
    liftIO $ execSchedule $
        addJob (runReaderT announcePoll h) "0 7 * * 5" -- TODO Make this easily configurable?
    return ()

announcePoll :: DiscordHandler ()
announcePoll = do
    pollUrl <- ReaderT (const createPoll)
    id      <- ReaderT (const (getEnv "CHAN_ID"))
    void $
      restCall $ do
        R.CreateMessage (read id) (pack pollUrl)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent

main :: IO ()
main = do
    token <- getEnv "TOKEN"
    userFacingError <- runDiscord $ def
             { discordToken = pack token
             , discordOnEvent = eventHandler
             , discordOnStart = startHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)
