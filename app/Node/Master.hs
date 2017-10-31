{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module Node.Master where

import           Protolude                                          hiding (state)

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet as SL
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)

import           Config
import           Node.Common
import           Node.Worker

remotable ['worker]

newtype MasterState = MasterState
    { _startedCount :: Int
    } deriving (Show)

handleStartedMessage :: MasterState -> StartedMessage -> Process MasterState
handleStartedMessage !state Started = do
    -- say $ "Received Started status, result: " <> showResult state
    let !newStarted = _startedCount state + 1
        !newState = state { _startedCount = newStarted }
    return newState

waitForAllNodesLoop :: Int -> MasterState -> Process ()
waitForAllNodesLoop !nodesCount !state = do
    state' <- receiveWait [ match $ handleStartedMessage state ]
    when (_startedCount state' < nodesCount) $
        waitForAllNodesLoop nodesCount state'

waitForAllNodes :: Int -> Process ()
waitForAllNodes nodesCount = waitForAllNodesLoop nodesCount $ MasterState 0


spawnNode :: MasterConfig -> NodeId -> [NodeId] -> (NodeId, Seed) -> Process ProcessId
spawnNode masterConfig masterNodeId nodeIds (nodeId, nodeSeed) = spawn nodeId ($(mkClosure 'worker) workerConfig)
  where
    workerConfig = WorkerConfig masterConfig' masterNodeId nodeIds
    masterConfig' = masterConfig { _seed = nodeSeed }

withSeedsFrom :: Seed -> [NodeId] -> [(NodeId, Seed)]
withSeedsFrom initSeed nodeIds = zip nodeIds seeds
  where
    seeds :: [Seed]
    seeds = iterate calcNextSeed initSeed
    calcNextSeed :: Seed -> Seed
    calcNextSeed s = fromIntegral $ fromIntegral s * (2654435761 :: Integer) `mod` ((2 :: Integer) ^ (32 :: Integer)) -- Knuth's multiplicative method

threadDelayS :: Int -> IO ()
threadDelayS = threadDelay . (* 1000000)

master :: Backend -> MasterConfig -> [NodeId] -> Process ()
master backend masterConfig@MasterConfig { _sendDuration = sendDuration, _waitDuration = waitDuration, _seed = seed } nodeIds = do
    masterPid <- getSelfPid
    let masterNodeId = processNodeId masterPid
    register masterService masterPid

    _processIds <- forM (withSeedsFrom seed nodeIds) $ spawnNode masterConfig masterNodeId nodeIds
    -- say $ "Slaves: "     <> show nodeIds
    -- say $ "Processes: "  <> show processIds
    -- self <- getSelfPid
    waitForAllNodes $ length nodeIds
    forM_ nodeIds $ \nodeId ->
        -- say $ "Sending start to node: " <> show nodeId
        nsendRemote nodeId senderService Start

    say $ "Config: " <> show masterConfig
    say $ "Sending for " <> show sendDuration <> " second(s)"
    liftIO $ threadDelayS sendDuration
    say $ "Waiting for " <> show waitDuration <> " second(s)"
    liftIO $ threadDelayS waitDuration
    SL.terminateAllSlaves backend
    say "Slaves terminated"

remoteTable :: RemoteTable
remoteTable = Node.Master.__remoteTable initRemoteTable
