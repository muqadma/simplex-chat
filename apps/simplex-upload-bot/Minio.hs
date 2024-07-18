{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, RecordWildCards #-}


module Minio where


--
-- MinIO Haskell SDK, (C) 2017-2019 MinIO, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

import           GHC.Generics
import           Network.Minio
import           Control.Monad.IO.Class
import           Data.Monoid           ((<>))
import           Data.Text             (pack)
import           Options.Applicative
import           System.FilePath.Posix
import           UnliftIO              (throwIO, try)
import qualified Data.Text as T
import Data.Text (Text)
import           Prelude
import Data.String

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

data StorageOpts = StorageOpts
  { bucket :: Text
  , endpoint :: Text
  }
  deriving (Generic)

-- optparse-applicative package based command-line parsing.
storageOpts :: Parser StorageOpts
storageOpts = StorageOpts
              <$> (strOption (long "minio-bucket" <> metavar "MINIO_BUCKET" <> help "the bucket name on GCS or a MinIO server"))
              <*> (strOption (long "minio-endpoint" <> metavar "MINIO_ENDPOINT" <> help "the endpoint to GCS or a MinIO server"))


toConnectInfo :: StorageOpts -> IO ConnectInfo
toConnectInfo StorageOpts{..} = do
  creds <- fromMinioEnv
  case creds of
    Nothing -> error "Creds Not loaded From Env"
    Just c -> return $ setCreds c
      $ setRegion "ap-southeast-1"
      $ fromString . T.unpack $ endpoint

getStorageOpts  = execParser $ info
            (helper <*> storageOpts)
            (fullDesc
             <> progDesc "FileUploader"
             <> header "FileUploader - a simple file-uploader program using minio-hs"
             <> forwardOptions
            )

uploadFile :: ConnectInfo -> Bucket -> FilePath -> IO (Either (FilePath, MinioErr) (FilePath, T.Text))
uploadFile c bucket f = do
  let o = takeBaseName f
      object = pack o
  res <- runMinio c $ fPutObject bucket object f defaultPutObjectOptions
  case res of
    Left e -> do
      putStrLn $ "upload failed for file: " <> takeBaseName f <> " with error: " <> (show e)
      return $ Left (f, e)
    Right _ -> do
      putStrLn $ "upload succeeded for file: " <> takeBaseName f <> " with object: " <> o
      return $ Right (f, object)


mkBucket :: ConnectInfo -> Bucket -> IO (Either MinioErr ())
mkBucket c bucket = runMinio c $ do
    -- Make a bucket; catch bucket already exists exception if thrown.
    bErr <- try $ makeBucket bucket Nothing
    -- If the bucket already exists, we would get a specific
    -- `ServiceErr` exception thrown.
    case bErr of
      Left BucketAlreadyOwnedByYou -> return ()
      Left e                       -> throwIO e
      Right _                      -> return ()
