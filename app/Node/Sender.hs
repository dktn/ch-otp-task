module Node.Sender
    ( sendWorker
    , senderService
    ) where

import           Protolude                   hiding (state)

import           Control.Distributed.Process (NodeId, Process, expect, getSelfPid, processNodeId, register)
import           System.Random               (StdGen, randomR)

import           Node.Common

sendNumbersLoop :: NodeId -> Timestamp -> StdGen -> [NodeId] -> Int -> Process ()
sendNumbersLoop selfNodeId stopTime gen nodeIds msgDelay = do
    now <- getCurrentTimeMicros
    when (now < stopTime) $ do
        let (val, gen') = randomR (0, 1) gen :: (Double, StdGen)
            msg = ValueMessage val (NodeTimestamp now selfNodeId)
        broadcastToAll receiverService msg nodeIds
        processDelay msgDelay
        sendNumbersLoop selfNodeId stopTime gen' nodeIds msgDelay

waitForStart :: Process ()
waitForStart = void (expect :: Process StartMessage)

sendStop :: [NodeId] -> Process ()
sendStop = broadcastToAll receiverService Finished

sendWorker :: Timestamp -> StdGen -> Int -> [NodeId] -> Process ()
sendWorker stopTime gen msgDelay nodeIds = do
    selfPid <- getSelfPid
    let selfNid = processNodeId selfPid
    register senderService selfPid
    waitForStart
    sendNumbersLoop selfNid stopTime gen nodeIds msgDelay
    sendStop nodeIds
