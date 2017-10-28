module Config where

import           Protolude

import           Control.Distributed.Process (NodeId)
import           Data.Binary
import           GHC.Generics
import           Options.Applicative

type Seed = Int
type Duration = Int

data HostConfig = HostConfig
    { host :: Text
    , port :: Text
    } deriving (Show)

data MasterConfig = MasterConfig
    { sendDuration :: Duration
    , waitDuration :: Duration
    , seed         :: Seed
    } deriving (Show, Generic, Binary)

data WorkerConfig = WorkerConfig
    { config  :: MasterConfig
    , nodeIds :: [NodeId]
    } deriving (Show, Generic, Binary)

data Options
    = MasterOptions HostConfig MasterConfig
    | SlaveOptions  HostConfig
    deriving (Show)
