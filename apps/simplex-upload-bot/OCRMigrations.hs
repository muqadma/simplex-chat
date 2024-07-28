{-# LANGUAGE QuasiQuotes, NamedFieldPuns, OverloadedStrings, TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module OCRMigrations where


import Database.SQLite.Simple (Query, fromQuery, Only(..), FromRow(..), ToRow(..), field) --,
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.QQ (sql)

import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Int

import Simplex.Chat.Store.Files
import Simplex.Chat.Types ()
import Simplex.Messaging.Agent.Store.SQLite (firstRow, firstRow', maybeFirstRow, SQLiteStore, createSQLiteStore, MigrationError, MigrationConfirmation)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Data.List (sortOn)
import qualified Data.Text as T
import Data.ByteArray (ScrubbedBytes)

createOcrStore :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> IO (Either MigrationError SQLiteStore)
createOcrStore dbPath key keepKey = createSQLiteStore dbPath key keepKey migrations

ocrStoreFile :: FilePath -> FilePath
ocrStoreFile = (<> "_ocr.db")


schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("create_ocr_documents", [sql|
      CREATE TABLE ocr_documents (
        ocr_id INTEGER PRIMARY KEY,
        file_id INTEGER NOT NULL,
        md_path Text,
        shared_message_id TEXT NOT NULL,
        chat_item_id TEXT NOT NULL,
        user_id INTEGER NOT NULL,
        datalab_request_id TEXT NOT NULL,
        ocr_status TEXT NOT NULL,
        mime_ty TEXT NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        agent_id INTEGER NOT NULL,
        chat_id TEXT NOT NULL
      )
    |], Just [sql|
      DROP TABLE ocr_documents
    |]),
    ("add_ocr_document_indexes", [sql|
      CREATE INDEX ocr_documents_file_id ON ocr_documents (file_id);
      CREATE INDEX ocr_documents_user_id ON ocr_documents (user_id);
      CREATE INDEX ocr_documents_chat_id ON ocr_documents (chat_id);
    |], Just [sql|
      DROP INDEX ocr_documents_file_id;
      DROP INDEX ocr_documents_user_id;
      DROP INDEX ocr_documents_chat_id;
    |])
  ]

type OCRId = Int64

data OCRStatus = OCRInitial
               | OCRRequesting
               | Processing
               | Complete
               | Error
               | UnknownStatus
  deriving (Eq, Ord, Enum)

instance Show OCRStatus where
  show OCRInitial = "initial"
  show OCRRequesting = "requesting"
  show Processing = "processing"
  show Complete = "complete"
  show Error = "error"
  show UnknownStatus = "unknown"

instance Read OCRStatus where
  readsPrec _ "initial" = [(OCRInitial, "")]
  readsPrec _ "requesting" = [(OCRRequesting, "")]
  readsPrec _ "processing" = [(Processing, "")]
  readsPrec _ "complete" = [(Complete, "")]
  readsPrec _ "error" = [(Error, "")]
  readsPrec _ "unknown" = [(UnknownStatus, "")]
  readsPrec _ _ = []

instance ToField OCRStatus where
  toField OCRInitial = toField ("initial" :: T.Text)
  toField OCRRequesting = toField ("requesting" :: T.Text)
  toField Processing = toField ("processing" :: T.Text)
  toField Complete = toField ("complete" :: T.Text)
  toField Error = toField ("error" :: T.Text)
  toField UnknownStatus = toField ("unknown" :: T.Text)

instance FromField OCRStatus where
  fromField f = do
    s <- fromField @String f
    case s of
      "initial" -> pure OCRInitial
      "requesting" -> pure OCRRequesting
      "processing" -> pure Processing
      "complete" -> pure Complete
      "error" -> pure Error
      _ -> pure UnknownStatus


data OCRRec = OCRRec
  { ocrId :: Int64
  , fileId :: Int64
  , mdPath :: T.Text
  , sharedMessageId :: T.Text
  , ocrChatItemId :: T.Text
  , ocrUserId :: UserId
  , datalabRequestId :: T.Text
  , ocrStatus :: T.Text
  , mimeTy :: T.Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , agentId :: Int
  , chatId :: T.Text
  }

instance FromRow OCRRec where
  fromRow = OCRRec <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow OCRRec where
  toRow OCRRec{..} =
    [ toField ocrId
    , toField fileId
    , toField mdPath
    , toField sharedMessageId
    , toField ocrChatItemId
    , toField ocrUserId
    , toField datalabRequestId
    , toField ocrStatus
    , toField mimeTy
    , toField createdAt
    , toField updatedAt
    , toField agentId
    , toField chatId
    ]



createOCRRec :: DB.Connection -> OCRRec -> IO ()
createOCRRec db orec = do
  createdAt <- getCurrentTime
  let updatedAt = createdAt
  DB.execute
    db
    [sql|
      INSERT INTO ocr_documents (
        ocr_id,
        file_id ,
        md_path Text,
        shared_message_id,
        chat_item_id,
        user_id,
        datalab_request_id,
        ocr_status,
        mime_ty,
        created_at,
        updated_at,
        agent_id,
        chat_id
      ) Values (? ,? ,? ,? ,? ,?,? ,? ,? ,? ,? ,?)
    |]
    orec



setOCRStatus :: DB.Connection -> OCRId -> OCRStatus -> IO ()
setOCRStatus db ocrId ocrStatus = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE ocr_documents
      SET ocr_status = ? , updated_at = ?
      WHERE ocr_id = ?
    |]
    (ocrStatus, updatedAt, ocrId)

getOCRRec :: DB.Connection -> OCRId -> IO (Maybe OCRRec)
getOCRRec db ocrId = do
  maybeFirstRow id $ DB.query db
    [sql|
      SELECT
        ocr_id,
        file_id,
        md_path,
        shared_message_id,
        chat_item_id,
        user_id,
        datalab_request_id,
        ocr_status,
        created_at,
        updated_at,
        agent_id,
        chat_id
      FROM ocr_documents
      WHERE ocr_id = ?
    |]
    (Only ocrId)

getFileOCRRec :: DB.Connection -> Int64 -> IO (Maybe OCRRec)
getFileOCRRec db fileId = do
  maybeFirstRow id $ DB.query db
    [sql|
      SELECT
        ocr_id,
        file_id,
        md_path,
        shared_message_id,
        chat_item_id,
        user_id,
        datalab_request_id,
        ocr_status,
        created_at,
        updated_at,
        agent_id,
        chat_id
      FROM ocr_documents
      WHERE file_id = ?
    |]
    (Only fileId)



-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}
