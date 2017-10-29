module Config where

import           Protolude

import           Control.Distributed.Process (NodeId)
import           Data.Binary

type Seed = Int
type Duration = Int

data HostConfig = HostConfig
    { _host :: Text
    , _port :: Text
    } deriving (Show)

data MasterConfig = MasterConfig
    { _sendDuration :: Duration
    , _waitDuration :: Duration
    , _seed         :: Seed
    } deriving (Show, Generic, Binary)

data WorkerConfig = WorkerConfig
    { _config  :: MasterConfig
    , _nodeIds :: [NodeId]
    } deriving (Show, Generic, Binary)

data Options
    = MasterOptions HostConfig MasterConfig
    | SlaveOptions  HostConfig
    deriving (Show)
