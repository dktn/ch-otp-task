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

initialSenderState :: SenderState
initialSenderState = SenderState 0 0.0


sendNumbersLoop :: Timestamp -> StdGen -> [NodeId] -> SenderState -> Process ()
sendNumbersLoop !stopTime !gen !nodeIds !(SenderState count total) = do
    -- liftIO $ threadDelay $ 29 * 100000 -- TODO: for tests, remove later
    -- liftIO $ threadDelay $ 1 * 100000 -- TODO: for tests, remove later
    liftIO $ threadDelay 0
    -- self <- getSelfPid
    now <- liftIO getCurrentTimeMicros
    let (val, gen') = randomR (0, 1) gen :: (Double, StdGen)
        msg = ValueMessage val now
    -- say $ "Sending to all nodes message: " <> show msg
    forM_ nodeIds $ \nodeId ->
        -- say $ "Sending to node: " <> show nodeId <> " message: " <> show msg
        nsendRemote nodeId receiverService msg
    now' <- liftIO getCurrentTimeMicros
    let !newCount = count + 1
        !newTotal = (total + val * fromIntegral count)
        !state' = SenderState newCount newTotal
    when (now' < stopTime) $ sendNumbersLoop stopTime gen' nodeIds state'
    -- when (now' >= stopTime) $ say $ "Sender final state: " <> show state'

sendStop :: [NodeId] -> Process ()
sendStop nodeIds =
    forM_ nodeIds $ \nodeId ->
        -- say "Sending STOP"
        nsendRemote nodeId receiverService Finished

sendWorker :: Timestamp -> StdGen -> [NodeId] -> Process ()
sendWorker stopTime gen nodeIds = do
    -- self <- getSelfPid
    -- say $ "SendWorker pid: " <> show self
    sendNumbersLoop stopTime gen nodeIds initialSenderState
    sendStop nodeIds
