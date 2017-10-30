{-# LANGUAGE BangPatterns #-}

module Node.Sender
    ( sendWorker
    ) where

import           Protolude                   hiding (state)

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process
import           System.Random

import           Node.Common


data SenderState = SenderState
    { _sentCounter :: !Int
    , _localSum    :: !Double
    } deriving (Show)

newtype InitSenderState = InitSenderState
    { _startedCount :: Int
    } deriving (Show)

initialSenderState :: SenderState
initialSenderState = SenderState 0 0.0


sendNumbersLoop :: Timestamp -> StdGen -> [NodeId] -> Int -> SenderState -> Process ()
sendNumbersLoop !stopTime !gen !nodeIds !msgDelay state@(SenderState _count _total) = do
    -- liftIO $ threadDelay $ 29 * 100000 -- TODO: for tests, remove later
    -- liftIO $ threadDelay $ 1 * 100000 -- TODO: for tests, remove later
    liftIO $ threadDelay msgDelay
    pid <- getSelfPid
    let nid = processNodeId pid
    now <- liftIO getCurrentTimeMicros
    let (val, gen') = randomR (0, 1) gen :: (Double, StdGen)
        msg = ValueMessage val (NodeTimestamp now nid)
    -- say $ "Sending to all nodes message: " <> show msg
    forM_ nodeIds $ \nodeId ->
        -- say $ "Sending to node: " <> show nodeId <> " message: " <> show msg
        nsendRemote nodeId receiverService msg
    now' <- liftIO getCurrentTimeMicros
    -- let !newCount = count + 1
    --     !newTotal = (total + val * fromIntegral count)
    --     !state' = SenderState newCount newTotal
    when (now' < stopTime) $ sendNumbersLoop stopTime gen' nodeIds msgDelay state
    -- when (now' >= stopTime) $ say $ "Sender final state: " <> show state'

handleStartedMessage :: InitSenderState -> StartedMessage -> Process InitSenderState
handleStartedMessage !state Started = do
    -- say $ "Received Started status, result: " <> showResult state
    let !newStarted = _startedCount state + 1
        !newState = state { _startedCount = newStarted }
    return newState

waitForAllNodesLoop :: Int -> InitSenderState -> Process ()
waitForAllNodesLoop !nodesCount !state = do
    state' <- receiveWait
        [ match $ handleStartedMessage state
        ]
    when (_startedCount state' < nodesCount) $
        waitForAllNodesLoop nodesCount state'

waitForAllNodes :: Int -> Process ()
waitForAllNodes nodesCount = waitForAllNodesLoop nodesCount $ InitSenderState 0

sendStop :: [NodeId] -> Process ()
sendStop nodeIds =
    forM_ nodeIds $ \nodeId ->
        -- say "Sending STOP"
        nsendRemote nodeId receiverService Finished

sendWorker :: Timestamp -> StdGen -> Int -> [NodeId] -> Process ()
sendWorker !stopTime !gen !msgDelay !nodeIds = do
    -- self <- getSelfPid
    -- say $ "SendWorker pid: " <> show self
    -- waitForAllNodes $ length nodeIds
    sendNumbersLoop stopTime gen nodeIds msgDelay initialSenderState
    sendStop nodeIds
