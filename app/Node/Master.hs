{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module Node.Master where

import           Protolude                                          hiding (state)

import           Control.Distributed.Process                        (NodeId, Process, ProcessId, RemoteTable,
                                                                     getSelfPid, match, processNodeId, receiveWait,
                                                                     register, say, spawn)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend, terminateAllSlaves)
import           Control.Distributed.Process.Closure                (mkClosure, remotable)
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
    let !startedCount' = _startedCount state + 1
    return $ state { _startedCount = startedCount' }

waitForAllNodesLoop :: Int -> MasterState -> Process ()
waitForAllNodesLoop !nodesCount !state = do
    state' <- receiveWait [ match $ handleStartedMessage state ]
    when (_startedCount state' < nodesCount) $
        waitForAllNodesLoop nodesCount state'

waitForAllNodes :: Int -> Process ()
waitForAllNodes nodesCount = waitForAllNodesLoop nodesCount $ MasterState 0

spawnWorker :: MasterConfig -> NodeId -> [NodeId] -> (NodeId, Seed) -> Process ProcessId
spawnWorker masterConfig masterNodeId nodeIds (nodeId, nodeSeed) = spawn nodeId ($(mkClosure 'worker) workerConfig)
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

spawnAllNodes :: MasterConfig -> NodeId -> [NodeId] -> Process ()
spawnAllNodes masterConfig masterNodeId nodeIds =
    forM_ (withSeedsFrom (_seed masterConfig) nodeIds) $ spawnWorker masterConfig masterNodeId nodeIds

waitAndTerminate :: Backend -> MasterConfig -> Process ()
waitAndTerminate backend masterConfig@MasterConfig { _sendDuration = sendDuration, _waitDuration = waitDuration } = do
    say $ "Config: "     <> show masterConfig
    say $ "Sending for " <> show sendDuration <> " second(s)"
    processDelaySec sendDuration
    say $ "Waiting for " <> show waitDuration <> " second(s)"
    processDelaySec waitDuration
    terminateAllSlaves backend
    say "Slaves terminated"

master :: Backend -> MasterConfig -> [NodeId] -> Process ()
master backend masterConfig nodeIds = do
    masterPid <- getSelfPid
    let masterNodeId = processNodeId masterPid
    register masterService masterPid
    spawnAllNodes masterConfig masterNodeId nodeIds
    waitForAllNodes $ length nodeIds
    broadcastToAll senderService Start nodeIds
    waitAndTerminate backend masterConfig

remoteTable :: RemoteTable
remoteTable = Node.Master.__remoteTable initRemoteTable
