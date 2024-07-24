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
import Options.Applicative
import Minio
import Network.Minio
import Fast



main :: IO ()
main = do
  UploadBotOpts{_ocrOpts, _storageOpts, _chatOpts}  <- welcomeGetOpts
  connInfo <- toConnectInfo _storageOpts
  simplexChatCore terminalChatConfig _chatOpts (uploadBot connInfo (bucket _storageOpts))



data UploadBotOpts = UploadBotOpts
  { _storageOpts :: StorageOpts
  , _chatOpts :: ChatOpts
  , _ocrOpts :: Token
  }

uploadBotOpts :: FilePath -> FilePath -> Parser UploadBotOpts
uploadBotOpts appDir defaultDBName = UploadBotOpts <$> storageOpts <*> (chatOptsP appDir defaultDBName) <*> ocrOpts
  where
    ocrOpts = strOption (long "datalab-token" <> metavar "OCR_TOKEN")

getUploadBotOpts :: FilePath -> FilePath -> IO UploadBotOpts
getUploadBotOpts appDir defaultDbFileName = execParser $
    info
      (helper <*> versionOption <*> uploadBotOpts appDir defaultDbFileName)
      (header versionStr <> fullDesc <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr


welcomeGetOpts :: IO UploadBotOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts <- getUploadBotOpts appDir "upload_bot"
  putStrLn $ "SimpleX + Minio Upload Bot v" ++ versionNumber
  let dbPrefix = dbFilePrefix . coreOptions . _chatOpts $ opts
  let fileFolder = optFilesFolder (_chatOpts opts)
  putStrLn $ "db: " <> dbPrefix <> "_chat.db, " <> dbPrefix <> "_agent.db"
  putStrLn $ "Files Folder: " <> show fileFolder
  pure opts

a </> b = a <> "\n" <> b

welcomeMessage :: String
welcomeMessage = "Welcome to the muqadma upload bot. Upload an image or a pdf file you want to run OCR on."
               </> "We use simplex-chat because it is not tied to an Internet Giant that farms your information. The data sent to the bot is only accessible to you and the collaborators you set."


identification :: String
identification = "Before we can get started, we need to establish your identity"
             </> "  1> write down your full name, then send"
             </> "  2> tell us the name of your firm and whether you want to invite anyone from it"
             </> "  3> take a picture of your Bar Id or CNIC and send it"


cases :: String
cases = "This is your case chat."
    </> "We will send you a message every time there's progress in any of your cases"
    </> "You can request a new document based on one of your templates."
    </> "Simply supply the facts and the particulars and we can review the draft together."

firm :: String
firm = "Here is the membership and financial metadata of your firm."
    </> "and a link to every case."


uploadBot :: ConnectInfo -> Bucket -> User -> ChatController -> IO ()
uploadBot conn bucket _user cc = do
  mkBucket conn bucket
  initializeBotAddress cc
  url <- getBotURL cc
  putStrLn $ "Bot url is: " <> url
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        sendMessage cc contact welcomeMessage
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = rc@(CIRcvMsgContent mc)}) -> do
        print $ "Received message from " <> (show contact)
        case mc of
          MCText t -> printT $ "Received text message: " <> t
          MCLink {text} -> printT $ "Received link message: " <> text
          MCImage {text} -> printT $ "Received image message: " <> text
          MCVideo {text} -> printT $ "Received video message: " <> text
          MCVoice {text} -> printT $ "Received voice message: " <> text
          MCFile text -> printT $ "Received file message: " <> text
          MCUnknown a b _ -> printT $ "Unknown Message Content Type:\n " <> a <> "\n" <> b
      CRContactSubSummary {user = User { userId
                                       , agentUserId
                                       , userContactId
                                       , localDisplayName
                                       }
                          , contactSubscriptions} -> putStrLn $ "contact sub summary:" <> (show userId) <> " " <> T.unpack localDisplayName <> " " <> (show userContactId)
      CRPendingSubSummary {user = User { userId
                                       , agentUserId
                                       , userContactId
                                       , localDisplayName
                                       }
                          , pendingSubscriptions} -> putStrLn $ "contact sub summary:" <> (show userId) <> " " <> T.unpack localDisplayName <> " " <> (show userContactId)
      a -> putStrLn $ "Received unknown message type: " <> show a
  where
    printT = putStrLn . T.unpack
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
