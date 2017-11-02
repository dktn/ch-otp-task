module Main where

import           Protolude

import           Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, startMaster, startSlave)
import           Options.Applicative                                (execParser, fullDesc, header, helper, info,
                                                                     progDesc)

import           Config
import           Node.Master
import           Options

sanitazeConfig :: MasterConfig -> MasterConfig
sanitazeConfig masterConfig =
    masterConfig
        { _sendDuration = max 1      (_sendDuration masterConfig)
        , _waitDuration = max 1      (_waitDuration masterConfig)
        , _msgDelay     = max 0      (_msgDelay     masterConfig)
        , _msgBuffer    = max 1000   (_msgBuffer    masterConfig)
        , _timeToShow   = max 100000 (_timeToShow   masterConfig)
        }

run :: Options -> IO ()
run (MasterOptions hostConfig origMasterConfig) = do
    backend <- initializeBackend (toS $ _host hostConfig) (toS $ _port hostConfig) remoteTable
    startMaster backend (master backend $ sanitazeConfig origMasterConfig)
run (SlaveOptions hostConfig) = do
    backend <- initializeBackend (toS $ _host hostConfig) (toS $ _port hostConfig) remoteTable
    startSlave backend

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Runs distributed task"
     <> header "ch-otp-task - CH/OTP Test Task" )
