{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import System.Directory (getAppUserDataDirectory)
import Text.Read

import Minio
import Network.Minio


main :: IO ()
main = do
  minioOpts <- getStorageOpts
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig opts (uploadBot (toConnectInfo minioOpts) (bucket minioOpts))


welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getChatOpts appDir "upload_bot"
  putStrLn $ "SimpleX + Minio Upload Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

a </> b = a <> "\n" <> b

welcomeMessage :: String
welcomeMessage = "Welcome to the muqadma upload bot. Upload an image or a pdf file you want to run OCR on."
               </> "We use simplex-chat because it is not tied to an Internet Giant that farms your information. The data sent to the bot is only accessible to you and the collaborators you set."

uploadBot :: ConnectInfo -> Bucket -> User -> ChatController -> IO ()
uploadBot conn bucket _user cc = do
  mkBucket conn bucket
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        sendMessage cc contact welcomeMessage
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = rc@(CIRcvMsgContent mc)}) -> do
        case mc of
          MCText t -> printT $ "Received text message: " <> t
          MCLink {text} -> printT $ "Received link message: " <> text
          MCImage {text} -> printT $ "Received image message: " <> text
          MCVideo {text} -> printT $ "Received video message: " <> text
          MCVoice {text} -> printT $ "Received voice message: " <> text
          MCFile text -> printT $ "Received file message: " <> text
          MCUnknown a b c -> printT $ "Unknown message type " <> a <> b
        let msg = T.unpack $ ciContentToText rc
            number_ = readMaybe msg :: Maybe Integer
        sendMessage cc contact $ case number_ of
          Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
          _ -> "\"" <> msg <> "\" is not a number"
      _ -> pure ()
  where
    printT = putStrLn . T.unpack
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
