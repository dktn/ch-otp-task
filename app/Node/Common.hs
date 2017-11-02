{-# LANGUAGE ConstraintKinds #-}

module Node.Common where

import           Protolude

import           Control.Distributed.Process (NodeId, Process, getSelfPid, nsendRemote, register)
import           Data.Binary
import           Data.Binary.Orphans         ()
import           Data.String                 (String)
import           Data.Time.Clock.POSIX
import           GHC.Generics                ()

type Timestamp = Integer

type Messagable a = (Typeable a, Binary a)

data NodeTimestamp = NodeTimestamp
    { _timestamp :: !Timestamp
    , _nodeId    :: !NodeId
    } deriving (Show, Eq, Ord, Generic, Binary, Hashable)

data StartedMessage  = Started  deriving (Show, Generic, Binary)
data StartMessage    = Start    deriving (Show, Generic, Binary)
data FinishedMessage = Finished deriving (Show, Generic, Binary)

data ValueMessage = ValueMessage
    { _nextValue     :: !Double
    , _nodeTimestamp :: !NodeTimestamp
    } deriving (Show, Eq, Ord, Generic, Binary, Hashable)

receiverService, senderService, masterService :: String
receiverService = "receiver"
senderService   = "sender"
masterService   = "master"

timeResolution :: Int
timeResolution = 1000000

timeResolutionPOSIX :: POSIXTime
timeResolutionPOSIX = 1000000

getCurrentTimeMicros :: Process Timestamp
getCurrentTimeMicros = liftIO $ numerator . toRational . (* timeResolutionPOSIX) <$> getPOSIXTime

broadcastToAll :: Messagable a => String -> a -> [NodeId] -> Process ()
broadcastToAll serviceName msg =
    mapM_ $ \nodeId ->
        nsendRemote nodeId serviceName msg

registerSelf :: String -> Process ()
registerSelf serviceName = register serviceName =<< getSelfPid

processDelay :: Int -> Process ()
processDelay = liftIO . threadDelay

processDelaySec :: Int -> Process ()
processDelaySec = processDelay . (* 1000000)
