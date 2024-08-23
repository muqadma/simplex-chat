{-# LANGUAGE NamedFieldPuns, DeriveGeneric #-}

module Controller where

import Simplex.Chat.Archive
import Simplex.Chat.Controller (ChatController (..), agentStore)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), closeSQLiteStore, keyString, sqlString, storeKey)
import Simplex.Messaging.Agent.Client (agentClientStore)

import Control.Monad.Reader
import UnliftIO.STM
import qualified Data.Text as T
import Data.Tree
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BL

import GHC.Generics

type Port = Int


data Store = Store
  { _chatStore :: SQLiteStore,
    _agentStore :: SQLiteStore,
    filesPath :: Maybe FilePath,
    assetsPath :: Maybe FilePath
  }


data MuqadmaController = MuqadmaController
  { firmChat :: ChatController
  , uploadChat :: ChatController
  , casesChat :: ChatController
  , researchChat :: ChatController
  , ocrStore :: SQLiteStore
  , ocrFilesPath :: Maybe FilePath
  , identityStore :: SQLiteStore
  , identityFilesPath :: Maybe FilePath
  , queues :: MuqadmaQs
  }

data MuqadmaNotifications = CaseNotification CaseEvents | FirmNotification FirmEvents | ResearchNotification ResearchEvents

data FirmEvents = FirmCreated | FirmUpdated | FirmDeleted

data ResearchEvents = NewResearch | ResearchUpdated | ResearchDeleted

type CaseId = Int

type DocumentId = Int

type Clause = T.Text

type Documents = Tree T.Text

type Annotations = Forest T.Text

data Case = Case { title :: T.Text
                 , book :: Tree Documents
                 , research :: Tree Documents
                 }

type Cases = M.Map CaseId Case

data Parties = Parties
  { prop :: [Clause]
  , opp :: [Clause]
  }

data CaseEvents = NewCase (Maybe Parties) (Maybe [CaseUpdate])
                | CaseUpdated CaseUpdate
                | CaseArchived CaseId
                | CaseDeleted CaseId

data CaseUpdate = Redraft DocumentId | UpdateCase CaseId

data MuqadmaQs = MuqadmaQs
  { uploadToFirm :: TBQueue FirmEvents
  , uploadToCases :: TBQueue CaseEvents
  , uploadToResearch :: TBQueue ResearchEvents
  }



-- | This is our Controller.
controllerStore :: ChatController -> IO Store
controllerStore ChatController {chatStore, filesFolder, assetsDirectory, smpAgent} = do
  let _agentStore = agentClientStore smpAgent
  filesPath <- readTVarIO filesFolder
  assetsPath <- readTVarIO assetsDirectory
  pure Store {_chatStore = chatStore, _agentStore, filesPath, assetsPath}


-- | This is our TimeStamping Server. Like in bitcoin.
-- | More accurately, we are following the UCL TIMESEC entry

data TSRequest = TSRequest
  { content :: BL.ByteString
  } deriving (Generic)

type TSTimed = (Integer, TSRequest)

data TSS = TSS
  { networkListener :: Port -> TBQueue TSRequest -> IO ()
  , requestTimer :: [TBQueue TSRequest] -> TBQueue TSTimed -> IO ()
  , roundQueueCoordinator :: TBQueue TSTimed -> TBQueue TSTimed -> IO ()
  }
