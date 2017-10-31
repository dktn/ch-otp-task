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

handleFinishedMessage :: ReceiverState -> FinishedMessage -> Process ReceiverState
handleFinishedMessage !state Finished = do
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

whenStr :: Bool -> String -> String
whenStr p str = if p then str else ""

receiveWorkerLoop :: Timestamp -> Int -> Int -> ReceiverState -> Process ()
receiveWorkerLoop !showTime !nodesCount !msgBuffer !state = do
    !stateMay <- receiveTimeout 500 -- TODO: handle by message
        [ match $ handleValueMessage msgBuffer state
        , match $ handleFinishedMessage state
        ]
    -- timer message
    now <- liftIO getCurrentTimeMicros
    let !state' = fromMaybe state stateMay
    if _finishedCount state' < nodesCount && now < showTime
        then receiveWorkerLoop showTime nodesCount msgBuffer state'
        else do
            -- calcStart <- liftIO getCurrentTimeMicros
            let !finalResult = calculateSumFromPQueue (_valuePQueue state') (_partialResult state')
            -- calcEnd <- liftIO getCurrentTimeMicros
            let !info = whenStr (_finishedCount state' == nodesCount) "all finished " <> whenStr (now >= showTime) "timeout "
                     <> whenStr (_skippedMsg state' > 0) (" skipped " <> show (_skippedMsg state'))
                    --  <> " result time " <> show (calcEnd - calcStart)
            say $ "Final result: " <> showResult finalResult <> " " <> info
            -- say $ "Final result: " <> showResult finalResult

receiveWorker :: Timestamp -> Int -> NodeId -> [NodeId] -> Process ()
receiveWorker !showTime !msgBuffer !masterNodeId !nodeIds = do
    -- self <- getSelfPid
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService =<< getSelfPid

    nsendRemote masterNodeId masterService Started

    receiveWorkerLoop showTime (length nodeIds) msgBuffer initialReceiverState
