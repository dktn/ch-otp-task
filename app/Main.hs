{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Protolude

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet as SL
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Options.Applicative

import           Config
import           Options
import           Worker

remotable ['worker]

spawnNode :: MasterConfig -> [NodeId] -> (NodeId, Seed) -> Process ProcessId
spawnNode masterConfig nodeIds (nodeId, nodeSeed) = spawn nodeId ($(mkClosure 'worker) workerConfig)
  where
    workerConfig = WorkerConfig config nodeIds
    config = masterConfig { seed = nodeSeed }

withSeedsFrom :: Seed -> [NodeId] -> [(NodeId, Seed)]
withSeedsFrom seed nodeIds = zip nodeIds seeds
  where
    seeds = iterate calcNextSeed seed
    calcNextSeed s = s * 2654435761 `mod` 2^32 -- Knuth's multiplicative method

master :: Backend -> MasterConfig -> [NodeId] -> Process ()
master backend masterConfig@(MasterConfig sendDuration waitDuration seed) nodeIds = do
    let sendDurationMs = 1000000 * sendDuration
        waitDurationMs = 1000000 * waitDuration
    processIds <- forM (withSeedsFrom seed nodeIds) $ spawnNode masterConfig nodeIds
    say $ "Slaves: " <> show nodeIds
    say $ "Processes: " <> show processIds
    say $ "Sending for " <> show sendDuration <> " second(s)"
    liftIO $ threadDelay sendDurationMs
    say $ "Waiting for " <> show waitDuration <> " second(s)"
    liftIO $ threadDelay waitDurationMs
    SL.terminateAllSlaves backend
    say "Slaves terminated"

remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

run :: Options -> IO ()
run (MasterOptions hostConfig origMasterConfig@MasterConfig{ sendDuration = sd, waitDuration = wd }) = do
    let masterConfig = origMasterConfig { sendDuration = max 1 sd, waitDuration = max 1 wd }
    -- putText $ "Master started: " <> show hostConfig <> " " <> show masterConfig
    backend <- initializeBackend (toS $ host hostConfig) (toS $ port hostConfig) remoteTable
    startMaster backend (master backend masterConfig)
    -- liftIO $ threadDelay 1000000
run (SlaveOptions hostConfig) = do
    -- putText $ "Slave started: " <> show hostConfig
    backend <- initializeBackend (toS $ host hostConfig) (toS $ port hostConfig) remoteTable
    startSlave backend

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Runs distributed task"
     <> header "ch-opt-task - CH/OPT Test Task" )
