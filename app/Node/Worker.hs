module Node.Worker
    ( worker
    ) where

import           Protolude

import           Control.Distributed.Process (Process, spawnLocal)
import           System.Random               (mkStdGen)

import           Config
import           Node.Common
import           Node.Receiver
import           Node.Sender

worker :: WorkerConfig -> Process ()
worker (WorkerConfig config masterNodeId nodeIds) = do
    now <- getCurrentTimeMicros
    let sendDurationPrec = toInteger (timeResolution * _sendDuration config)
        waitDurationPrec = toInteger (timeResolution * _waitDuration config)
        showDurationPrec = waitDurationPrec - toInteger (_timeToShow config)
        stopTime = now      + sendDurationPrec
        showTime = stopTime + showDurationPrec
    void . spawnLocal $ receiveWorker showTime (_msgBuffer config) masterNodeId nodeIds
    let gen = mkStdGen $ _seed config
    sendWorker stopTime gen (_msgDelay config) nodeIds
