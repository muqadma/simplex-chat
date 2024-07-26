{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Simplex.Chat.Types hiding (ContactRef(..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Chat.Store.Files (getLocalCryptoFile)
import System.Directory (getAppUserDataDirectory)
import Text.Read
import Options.Applicative
import Minio
import Network.Minio
import Fast
import Controller

import Data.Int

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


determineMimeType c = "unknown"
    return "unknown"

{-
getLocalCryptoFile :: DB.Connection -> UserId -> Int64 -> Bool -> ExceptT StoreError IO CryptoFile
getLocalCryptoFile db userId fileId sent =
-}

getCryptoFile :: UserId -> Int64 -> Bool -> CM (CF.CryptoFile)
getCryptoFile userId fileId sent = withStore (\c -> getLocalCryptoFile c userId fileId sent)


onNewChatItem :: MsgContent -> IO ()
onNewChatItem (MCText text) = printT $ "Received text message: " <> text
onNewChatItem MCLink {text} = printT $ "Received link message: " <> text
onNewChatItem MCImage {text} = printT $ "Received image message: " <> text
onNewChatItem MCVideo {text} = printT $ "Received video message: " <> text
onNewChatItem MCVoice {text} = printT $ "Received voice message: " <> text
onNewChatItem (MCFile text) = printT $ "Received file message: " <> text
onNewChatItem (MCUnknown a b _) = printT $ "Unknown Message Content Type:\n " <> a <> "\n" <> b

ocrMsgContent :: MsgContent -> IO (SharedMsgId)
ocrMsgContent (MCText text) = printT $ "Received text message: " <> text


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
        print $ "Direct Chat from: " <> (show $ Simplex.Chat.Types.contactId contact) <> " - with content: " <> (show mc)
        onNewChatItem mc
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
      CRRcvFileDescrReady { user, chatItem } -> do
        let ty = determineMimeType chatItem
        path <- determineMessagePath chatItem
        putStrLn $ "Received file of type: " <> show ty
      a -> putStrLn $ "Received unknown message type: " <> show a
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
    determineMessagePath c = do
      return $ "unknown"


printT = putStrLn . T.unpack
