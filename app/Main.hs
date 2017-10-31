{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Protolude

import           Control.Distributed.Process.Backend.SimpleLocalnet as SL
import           Options.Applicative

import           Config
import           Node.Master
import           Options

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
