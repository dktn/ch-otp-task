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
import           Node.Worker
import           Options

remotable ['worker]

spawnNode :: MasterConfig -> [NodeId] -> (NodeId, Seed) -> Process ProcessId
spawnNode masterConfig nodeIds (nodeId, nodeSeed) = spawn nodeId ($(mkClosure 'worker) workerConfig)
  where
    workerConfig = WorkerConfig masterConfig' nodeIds
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
    _processIds <- forM (withSeedsFrom seed nodeIds) $ spawnNode masterConfig nodeIds
    -- say $ "Slaves: "     <> show nodeIds
    -- say $ "Processes: "  <> show processIds
    say $ "Config: " <> show masterConfig
    say $ "Sending for " <> show sendDuration <> " second(s)"
    liftIO $ threadDelayS sendDuration
    say $ "Waiting for " <> show waitDuration <> " second(s)"
    liftIO $ threadDelayS waitDuration
    SL.terminateAllSlaves backend
    say "Slaves terminated"

remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

run :: Options -> IO ()
run (MasterOptions hostConfig origMasterConfig@MasterConfig{ _sendDuration = sd, _waitDuration = wd }) = do
    let masterConfig = origMasterConfig { _sendDuration = max 1 sd, _waitDuration = max 1 wd }
    -- putText $ "Master started: " <> show hostConfig <> " " <> show masterConfig
    backend <- initializeBackend (toS $ _host hostConfig) (toS $ _port hostConfig) remoteTable
    startMaster backend (master backend masterConfig)
    -- liftIO $ threadDelay 1000000
run (SlaveOptions hostConfig) = do
    -- putText $ "Slave started: " <> show hostConfig
    backend <- initializeBackend (toS $ _host hostConfig) (toS $ _port hostConfig) remoteTable
    startSlave backend

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Runs distributed task"
     <> header "ch-opt-task - CH/OPT Test Task" )
