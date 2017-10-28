module Worker
    ( worker
    ) where

import           Protolude                                          hiding (State)

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet as SL
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad.Loops
import           Data.Binary
import           Data.Binary.Orphans
import           Data.String                                        (String)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           System.Random

import           Config

receiverService :: String
receiverService = "receiver"

data Status = Finished deriving (Show, Generic, Binary)

data ValueMessage = ValueMessage
    { nextValue :: Double
    , timestamp :: UTCTime
    } deriving (Show, Generic, Binary)

data State = State
    { currentSum      :: Double
    , receivedNumbers :: Integer
    , latestTimeStamp :: UTCTime
    } deriving (Show)

showResult :: State -> String
showResult (State s m _) = "<" <> show m <> ", " <> show s <> ">"

-- Receiver ---------------------------------------------------------------

handleValueMessage :: State -> ValueMessage -> Process State
handleValueMessage state@(State sumVals m ts) msg@(ValueMessage newVal newTs) = do
    say $ "Received message: " <> show msg
    if ts < newTs
        then return $ State (sumVals + fromIntegral m * newVal) (m + 1) newTs
        else return state

handleStatus :: State -> Status -> Process State
handleStatus state Finished = do
    say $ "Received Finished status, result: " <> showResult state
    return state

receiveWorkerLoop :: State -> Process ()
receiveWorkerLoop state = do
    state' <- receiveWait
        [ match $ handleValueMessage state
        , match $ handleStatus state
        ]
    receiveWorkerLoop state'

receiveWorker :: Process ()
receiveWorker = do
    self <- getSelfPid
    let zeroTime = posixSecondsToUTCTime 0
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService self
    receiveWorkerLoop $ State 0.0 0 zeroTime

    return ()

-- Sender ---------------------------------------------------------------

sendingAllowed :: MonadIO m => UTCTime -> m Bool
sendingAllowed stopTime = do
    now <- liftIO getCurrentTime
    return $ now < stopTime

sendNumbersUntil :: UTCTime -> StdGen -> [NodeId] -> Process ()
sendNumbersUntil stopTime gen nodeIds = do
    self <- getSelfPid
    now <- liftIO getCurrentTime
    let (val, gen') = randomR (0, 1) gen :: (Double, StdGen)
        msg = ValueMessage val now
        -- say $ show self <> " Random val: " <> show n
        -- say $ show self <> " Random gen: " <> show gen'
    liftIO $ threadDelay 500000 -- TODO: for tests, remove later
    forM_ nodeIds $ \nodeId ->
        -- say $ "Sending to node: " <> show nodeId <> " value: " <> show n
        nsendRemote nodeId receiverService msg
    now <- liftIO getCurrentTime
    when (now < stopTime) $
        sendNumbersUntil stopTime gen' nodeIds

sendStop :: [NodeId] -> Process ()
sendStop nodeIds =
    forM_ nodeIds $ \nodeId ->
        -- say "Sending STOP"
        nsendRemote nodeId receiverService Finished

sendWorker :: UTCTime -> StdGen -> [NodeId] -> Process ()
sendWorker stopTime gen nodeIds = do
    -- self <- getSelfPid
    -- say $ "SendWorker pid: " <> show self
    sendNumbersUntil stopTime gen nodeIds
    sendStop nodeIds

-- Worker ---------------------------------------------------------------

worker :: WorkerConfig -> Process ()
worker workerConfig@(WorkerConfig config nodeIds) = do
    -- say $ "Worker config: " <> show workerConfig
    now <- liftIO getCurrentTime
    let stopTime = addUTCTime (fromIntegral $ sendDuration config) now

    say $ "Current time: " <> show now
    say $ "Stop    time: " <> show stopTime

    let gen = mkStdGen $ seed config
    spawnLocal $ sendWorker stopTime gen nodeIds

    receiveWorker

    -- liftIO $ threadDelay 3000000
