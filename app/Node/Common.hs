{-# LANGUAGE BangPatterns #-}

module Node.Common where

import           Protolude

import           Control.Distributed.Process (NodeId)
import           Data.Binary
import           Data.Binary.Orphans         ()
import           Data.String                 (String)
import           Data.Time.Clock.POSIX
import           GHC.Generics                ()

type Timestamp = Integer

-- Timestamp is not enough for determinism taking into account limited clock resolution
-- Also "Time, Clocks, nad the Ordering of Events in a Distributed System" Leslie Lamport, p. 561

data NodeTimestamp = NodeTimestamp
    { _timestamp :: !Timestamp
    , _nodeId    :: !NodeId
    } deriving (Show, Eq, Ord, Generic, Binary, Hashable)

data StartedMessage = Started deriving (Show, Generic, Binary)

data StartMessage = Start deriving (Show, Generic, Binary)

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

getCurrentTimeMicros :: IO Timestamp
getCurrentTimeMicros = numerator . toRational . (* timeResolutionPOSIX) <$> getPOSIXTime

