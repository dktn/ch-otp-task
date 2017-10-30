{-# LANGUAGE BangPatterns #-}

module Node.Receiver
    ( receiveWorker
    ) where

import           Protolude                   hiding (state)

import           Control.Distributed.Process
import           Data.HashPSQ                (HashPSQ)
import qualified Data.HashPSQ                as PQ
import           Data.String                 (String)
import           Prelude                     (error)

import           Node.Common


type ValuePQueue = HashPSQ ValueMessage NodeTimestamp Double

data Result = Result
    { _currentSum   :: !Double
    , _currentCount :: !Integer
    } deriving (Show)

data ReceiverState = ReceiverState
    { _partialResult   :: !Result
    , _latestTimeStamp :: !Timestamp
    , _pqueueSize      :: !Int
    , _valuePQueue     :: !ValuePQueue
    , _finishedCount   :: !Int
    , _skippedMsg      :: !Integer
    } deriving (Show)

-- IDEAS:
-- - multiple queues
-- - timer message

initialReceiverState :: ReceiverState
initialReceiverState = ReceiverState (Result 0.0 0) 0 0 PQ.empty 0 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

handleValueMessage :: Int -> ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage !msgBuffer state@(ReceiverState result@(Result !curSum !count) _lastTs pqsize !pq !fc !sk) msg@(ValueMessage newVal nodeTs@(NodeTimestamp newTs _nodeId)) =
    -- say $ "Received message: " <> show msg
    if pqsize < msgBuffer
        then do
            -- say "Add to pq"
            let !pq' = PQ.insert msg nodeTs newVal pq
                !newPQSize = pqsize + 1
                !newState = ReceiverState result newTs newPQSize pq' fc sk
            return newState
        else
            case PQ.minView pq of
                Just (_, nts@(NodeTimestamp _ts _nid), val, pq') ->
                    if nts < nodeTs
                        then do
                            -- say $ "Remove from pq val " <> show val <> " ts " <> show ts
                            let !pq'' = PQ.insert msg nodeTs newVal pq'
                                !newCount = count + 1
                                !newSum = curSum + fromIntegral newCount * val
                                !newResult = Result newSum newCount
                                !newState = ReceiverState newResult newTs pqsize pq'' fc sk
                            return newState
                        else do -- skip message :(
                            -- say "message skipped"
                            let !newSk = sk + 1
                            return $ state { _skippedMsg = newSk }
                Nothing -> error "can't be here" -- return state

handleStatusMessage :: ReceiverState -> StatusMessage -> Process ReceiverState
handleStatusMessage !state Finished = do
    -- say $ "Received Finished status, result: " <> showResult state
    let !newFinishedCount = _finishedCount state + 1
        !newState = state { _finishedCount = newFinishedCount }
    return newState

calculateSumFromPQueue :: ValuePQueue -> Result -> Result
calculateSumFromPQueue !pq !partialResult = result
  where
    !pl = PQ.toList pq
    !result = foldl' calcSum partialResult pl
    calcSum (Result !s !m) (_, _, !v) = Result (s + v) (m + 1)

receiveWorkerLoop :: Timestamp -> Int -> Int -> ReceiverState -> Process ()
receiveWorkerLoop !showTime !nodesCount !msgBuffer !state = do
    !stateMay <- receiveTimeout 50
        [ match $ handleValueMessage msgBuffer state
        , match $ handleStatusMessage state
        ]
    -- timer message
    now <- liftIO getCurrentTimeMicros
    let !state' = fromMaybe state stateMay
    -- if now < showTime
    if _finishedCount state' < nodesCount && now < showTime
        then receiveWorkerLoop showTime nodesCount msgBuffer state'
        else do
            -- say $ "All nodes finished: " <> show (_finishedCount state' == nodesCount) <> " Time is out: " <> show (now >= showTime)
            let !finalResult = calculateSumFromPQueue (_valuePQueue state') (_partialResult state')
            -- say $ "All nodes finished: " <> show (_finishedCount state' == nodesCount) <> " Time is out: " <> show (now >= showTime)
            --    <> " Final state: " <> show state'
            say $ "Final result: " <> showResult finalResult <> " skipped: " <> show (_skippedMsg state')

receiveWorker :: Timestamp -> Int -> Int -> Process ()
receiveWorker !showTime !nodesCount !msgBuffer = do
    self <- getSelfPid
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService self
    receiveWorkerLoop showTime nodesCount msgBuffer initialReceiverState
