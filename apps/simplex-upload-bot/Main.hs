{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Simplex.Chat (receiveFile', toFSFilePath)
import qualified System.FilePath as FP
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages hiding (CIFileInfo(..))
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types hiding (ContactRef(..))
import Simplex.Chat.Markdown
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Chat.Store.Files (getLocalCryptoFile)
import Simplex.Chat.Store
import OCRMigrations

import System.Directory (getAppUserDataDirectory, removeFile, createDirectoryIfMissing)
import System.IO.Temp (withTempFile)
import System.IO (hClose)
import Text.Read
import Options.Applicative
import Minio
import Network.Minio
import Fast
import Controller
import Network.Mime
import Data.Int



main :: IO ()
main = do
  UploadBotOpts{_ocrOpts, _storageOpts, _chatOpts}  <- welcomeGetOpts
  connInfo <- toConnectInfo _storageOpts
  simplexChatCore terminalChatConfig _chatOpts (uploadBot _ocrOpts connInfo (bucket _storageOpts))


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



onNewChatItem :: MsgContent -> IO ()
onNewChatItem (MCText text) = printT $ "Received text message: " <> text
onNewChatItem MCLink {text} = printT $ "Received link message: " <> text
onNewChatItem MCImage {text} = printT $ "Received image message: " <> text
onNewChatItem MCVideo {text} = printT $ "Received video message: " <> text
onNewChatItem MCVoice {text} = printT $ "Received voice message: " <> text
onNewChatItem (MCFile text) = printT $ "Received file message: " <> text
onNewChatItem (MCUnknown a b _) = printT $ "Unknown Message Content Type:\n " <> a <> "\n" <> b

ocrMsgContent :: MsgContent -> Bool
ocrMsgContent (MCText text) = False
ocrMsgContent MCLink {text} = False
ocrMsgContent MCImage {text, image} = True
ocrMsgContent MCVideo {text} = False
ocrMsgContent MCVoice {text} = False
ocrMsgContent (MCFile a) = True
ocrMsgContent (MCUnknown a b _) = False

encryptedFile' :: UserId -> Int64 -> Bool -> CM (CF.CryptoFile)
encryptedFile' userId fileId sent = withStore $ \c -> getLocalCryptoFile c userId fileId sent
-- CIFileInfo

runCC :: forall a. ChatController -> CM a -> IO (Either ChatError a)
runCC cc = flip runReaderT cc . runExceptT

encryptedFile cc u x s = runCC cc (encryptedFile' u x s)



uploadBot :: Token -> ConnectInfo -> Bucket -> User -> ChatController -> IO ()
uploadBot (Token _ocrOpts) conn bucket _user cc = do
  store <- createOcrStore "./ocr.db" "123456" True >>= \case
    Left e -> error $ "Error creating OCR Store: " <> show e
    Right a -> return a
  mkBucket conn bucket
  initializeBotAddress cc
  url <- getBotURL cc
  putStrLn $ "Bot url is: " <> url
  dir <- (\f -> f FP.</> "ocrWorkDir")
    <$> (fromMaybe "./tmp" <$> readTVarIO (filesFolder cc))
  createDirectoryIfMissing True dir
  print $ "OCR Dir is: " <> dir
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        sendMessage cc contact welcomeMessage
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = rc@(CIRcvMsgContent mc), meta, file}) -> do
        print $ file
        print $ "Direct Chat from: " <> (show $ Simplex.Chat.Types.contactId contact) <> " - with content: " <> (show mc)
        onNewChatItem mc
      CRContactSubSummary {user = User { userId
                                       , userContactId
                                       , localDisplayName
                                       }
                          } -> putStrLn $ "contact sub summary:" <> (show userId) <> " " <> T.unpack localDisplayName <> " " <> (show userContactId)
      CRUserContactSubSummary {user = User { userId
                                           , userContactId
                                           , localDisplayName
                                           }
                          } -> putStrLn $ "contact user sub summary:" <> (show userId) <> " " <> T.unpack localDisplayName <> " " <> (show userContactId)
      CRPendingSubSummary {user = User { userId
                                       , userContactId
                                       , localDisplayName
                                       }
                          } -> putStrLn $ "contact sub summary:" <> (show userId) <> " " <> T.unpack localDisplayName <> " " <> (show userContactId)
      CRRcvFileDescrReady { user, chatItem, rcvFileTransfer, rcvFileDescr } -> do
        (either print print) =<< (runCC cc $ receiveFile' user rcvFileTransfer False Nothing Nothing)
      CRRcvFileAccepted { user, chatItem } -> do
        print $ "recieve file accepted"
      CRRcvFileStart { user, chatItem } -> do
        print $ "recieve file started"
      CRRcvFileProgressXFTP { user, chatItem_, receivedSize, totalSize, rcvFileTransfer  } -> do
        print $ "recieve file progress"
      CRRcvFileComplete { user, chatItem=(AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = rc@(CIRcvMsgContent mc), meta, file}) } -> do
        let
            User{userId} = user
            fid' :: Maybe Int64
            fid' = Simplex.Chat.Messages.fileId <$> file
            fname' :: Maybe T.Text
            fname' = T.pack . Simplex.Chat.Messages.fileName <$> file
            ty' = mimeByExt defaultMimeMap defaultMimeType <$> fname'
        case (,,) <$> fid' <*> fname' <*> ty' of
          Just (fi, fname, ty) -> do
            m' <- awaitCompletion dir userId fi fname ty
            case m' of
              Nothing -> print "Fuck why is there no markdown"
              Just m -> do
                print "Sending Message"
                mapM (sendMessage cc contact . T.unpack) (splitMessages m)
                print "Sent Message"
          Nothing -> do
            print "No File exists for ChatItem"
      a -> putStrLn $ "Received unknown message type: " <> show a
  where
    splitMessages :: T.Text -> [T.Text]
    splitMessages = T.chunksOf maxEncodedMsgLength
    awaitCompletion :: FilePath -> UserId -> Int64 -> T.Text -> BS.ByteString -> IO (Maybe T.Text)
    awaitCompletion dir userId fi fname ty = do
      cf <- encryptedFile cc userId fi False
      case cf of
        Left e -> do
          print $ "Error opening encrypted file" <> show e
          return Nothing
        Right (CF.CryptoFile filePath cfArgs) -> do
          liftIO $ putStrLn $ "Received file of type: " <> show ty
          bs <- runExceptT $ do
            fsFilePath <- Control.Monad.Reader.lift . (flip runReaderT) cc $ toFSFilePath filePath
            let src = CF.CryptoFile fsFilePath cfArgs
            CF.readFile src
          case bs of
            Left e -> (print $ "Error decrypting file: " <> show e) >> return Nothing
            Right contentBS -> doOCR dir fi fname ty contentBS
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
    doOCR :: FilePath -> Int64 -> T.Text -> BS.ByteString -> LBS.ByteString -> IO (Maybe T.Text)
    doOCR dir fileId fileName mimeType content = do
      withTempFile dir (T.unpack fileName) $ \ (f :: FilePath) h -> do
        print "In withTmpFile"
        BS.hPut h (BS.toStrict $ content)
        hClose h
        c <- withConfig _ocrOpts $ flip marker' f
        case c of
          Nothing -> do
            putStrLn "No Marker Response!"
            return Nothing
          Just (MarkerFinalResponse status markdown images meta success err npages) -> do
            let actualErr = case err of
                              Just "" -> Nothing
                              Just e -> Just e
                              Nothing -> Nothing
            case actualErr of
              Just e -> do
                putStrLn $ "Error: " <> show e
                return markdown
              Nothing -> do
                putStrLn $ "Success!"
                putStrLn $ "Meta: " <> show meta
                putStrLn $ "Status: " <> show status
                putStr $ "Pages: " <> show markdown
                putStrLn $ "Success: " <> show success
                putStrLn $ "Number of Pages: " <> show npages
                return $ markdown

printT = putStrLn . T.unpack
