{-# LANGUAGE QuasiQuotes #-}

module OCRMigrations where


import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)


schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("create_ocr_documents", [sql|
      CREATE TABLE ocr_documents (
        id INTEGER PRIMARY KEY,
        file_id INTEGER NOT NULL,
        shared_message_id TEXT NOT NULL,
        chat_item_id TEXT NOT NULL,
        user_id INTEGER NOT NULL,
        datalab_request_id TEXT NOT NULL,
        status TEXT NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        agent_id INTEGER NOT NULL,
        chat_id TEXT NOT NULL,
      )
    |], Just [sql|
      DROP TABLE ocr_documents
    |])
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}
