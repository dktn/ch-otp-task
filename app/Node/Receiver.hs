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


type ValuePQueue = HashPSQ ValueMessage Timestamp Double

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
    } deriving (Show)

-- IDEAS:
-- - multiple queues
-- - timer message

maxPQSize :: Int
maxPQSize = 5000

initialReceiverState :: ReceiverState
initialReceiverState = ReceiverState (Result 0.0 0) 0 0 PQ.empty 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

handleValueMessage :: ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage state@(ReceiverState result@(Result !curSum !count) _lastTs pqsize !pq fc) msg@(ValueMessage newVal newTs) =
    -- say $ "Received message: " <> show msg
    if pqsize < maxPQSize
        then do
            -- say "Add to pq"
            let !pq' = PQ.insert msg newTs newVal pq
                !newPQSize = pqsize + 1
                !newState = ReceiverState result newTs newPQSize pq' fc
            return newState
        else
            -- check if the new message timestamp is higher than the minimum!!!!!
            case PQ.minView pq of
                Just (_, ts, val, pq') ->
                    if ts < newTs
                        then do
                            -- say $ "Remove from pq val " <> show val <> " ts " <> show ts
                            let !pq'' = PQ.insert msg newTs newVal pq'
                                !newCount = count + 1
                                !newSum = curSum + fromIntegral newCount * val
                                !newResult = Result newSum newCount
                                !newState = ReceiverState newResult newTs pqsize pq'' fc
                            return newState
                        else do -- skip message :(
                            say "message skipped"
                            return state
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

receiveWorkerLoop :: Timestamp -> Int -> ReceiverState -> Process ()
receiveWorkerLoop showTime nodesCount state = do
    !stateMay <- receiveTimeout 1000
        [ match $ handleValueMessage state
        , match $ handleStatusMessage state
        ]
    -- timer message
    now <- liftIO getCurrentTimeMicros
    let !state' = fromMaybe state stateMay
    if _finishedCount state' < nodesCount || now < showTime
        then receiveWorkerLoop showTime nodesCount state'
        else do
            let finalResult = calculateSumFromPQueue (_valuePQueue state') (_partialResult state')
            -- say $ "All nodes finished: " <> show (_finishedCount state' == nodesCount) <> " Time is out: " <> show (now >= showTime)
            --    <> " Final state: " <> show state'
            say $ "Final result: " <> showResult finalResult

receiveWorker :: Timestamp -> Int -> Process ()
receiveWorker showTime nodesCount = do
    self <- getSelfPid
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService self
    receiveWorkerLoop showTime nodesCount initialReceiverState
