module Node.Common where

import           Protolude

import           Data.Binary
import           Data.Binary.Orphans   ()
import           Data.String           (String)
import           Data.Time.Clock.POSIX
import           GHC.Generics          ()

type Timestamp = Integer

data StatusMessage = Finished deriving (Show, Generic, Binary)

data ValueMessage = ValueMessage
    { _nextValue :: Double
    , _timestamp :: Timestamp
    } deriving (Show, Eq, Ord, Generic, Binary, Hashable)

receiverService :: String
receiverService = "receiver"

timeResolution :: Int
timeResolution = 1000000

timeResolutionPOSIX :: POSIXTime
timeResolutionPOSIX = 1000000

getCurrentTimeMicros :: IO Timestamp
getCurrentTimeMicros = numerator . toRational . (* timeResolutionPOSIX) <$> getPOSIXTime

timeToShowResult :: Int
timeToShowResult = timeResolution `div` 5

