module Node.Worker
    ( worker
    ) where

import           Protolude

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process
import           System.Random

import           Config
import           Node.Common
import           Node.Receiver
import           Node.Sender


worker :: WorkerConfig -> Process ()
worker (WorkerConfig config nodes) = do
-- worker workerConfig@(WorkerConfig config nodes) = do
    -- say $ "Worker config: " <> show config
    now <- liftIO getCurrentTimeMicros
    let sendDurationPrec = toInteger (timeResolution * _sendDuration config)
        waitDurationPrec = toInteger (timeResolution * _waitDuration config)
        showDurationPrec = waitDurationPrec - toInteger (_timeToShow config)
        stopTime = now      + sendDurationPrec
        showTime = stopTime + showDurationPrec
        -- killTime = stopTime + waitDurationPrec
    -- say $ "Current time: " <> show now
    -- say $ "Stop    time: " <> show stopTime
    -- say $ "Show    time: " <> show showTime
    -- say $ "Kill    time: " <> show killTime

    void $ spawnLocal $ receiveWorker showTime (length nodes) $ _msgBuffer config

    liftIO $ threadDelay 200000
    let gen = mkStdGen $ _seed config
    sendWorker stopTime gen nodes $ _msgDelay config
