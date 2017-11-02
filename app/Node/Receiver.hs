{-# LANGUAGE BangPatterns #-}

module Node.Receiver
    ( receiveWorker
    ) where

import           Protolude                   hiding (state)

import           Control.Distributed.Process
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.HashPSQ                (HashPSQ)
import qualified Data.HashPSQ                as PQ
import           Data.String                 (String)
import           Prelude                     (error)

import           Node.Common

type ValuePQueue     = HashPSQ ValueMessage NodeTimestamp Double

type MinTimestampMap = HashMap NodeId NodeTimestamp

data Result = Result
    { _currentSum   :: !Double
    , _currentCount :: !Integer
    } deriving (Show)

data ReceiverState = ReceiverState
    { _partialResult :: !Result
    , _pqueueSize    :: !Int
    , _maxpqueueSize :: !Int
    , _valuePQueue   :: !ValuePQueue
    , _minTimestamps :: !MinTimestampMap
    , _finishedCount :: !Int
    , _skippedMsg    :: !Integer
    } deriving (Show)

-- IDEAS:
-- - timer message

initialMinTimestampMap :: [NodeId] -> MinTimestampMap
initialMinTimestampMap nodeIds = HM.fromList $ initWithZeroTimestamp <$> nodeIds
  where
    initWithZeroTimestamp nodeId = (nodeId, NodeTimestamp 0 nodeId)

initialReceiverState :: [NodeId] -> ReceiverState
initialReceiverState nodeIds = ReceiverState (Result 0.0 0) 0 0 PQ.empty (initialMinTimestampMap nodeIds) 0 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

reduceBuffer :: NodeTimestamp -> (Result, ValuePQueue) -> (Result, ValuePQueue)
reduceBuffer minNodeTs resultPQueue@(result, pq) =
    case PQ.minView pq of
        Just (_, nts, val, pq') ->
            if nts < minNodeTs
                then reduceBuffer minNodeTs (addValToResult result val, pq')
                else resultPQueue
        Nothing -> error "can't be here"

{-# INLINE reduceBufferForState #-}
reduceBufferForState :: NodeTimestamp -> ReceiverState -> ReceiverState
reduceBufferForState minNodeTs state@(ReceiverState result@(Result _ !count) !pqsize !maxpqsize !pq !mt !fc !sk)
    | pqsize == 0 = state
    | otherwise =
            let (!newResult@(Result _ count'), !pq') = reduceBuffer minNodeTs (result, pq)
                reducedNum = count' - count
                newPQSize = pqsize - fromIntegral reducedNum
                !state' = ReceiverState newResult newPQSize maxpqsize pq' mt fc sk
            in state'

{-# INLINE addValToResult #-}
addValToResult :: Result -> Double -> Result
addValToResult (Result !s !m) !val = let !newM = m + 1 in Result (s + fromInteger newM * val) newM

{-# INLINE addNewMsgToPQueue #-}
addNewMsgToPQueue :: ReceiverState -> MinTimestampMap -> ValueMessage -> ReceiverState
addNewMsgToPQueue (ReceiverState !result !pqsize !maxpqsize !pq _mt !fc !sk) !mt' msg@(ValueMessage !newVal !nodeTs) = newState
  where
    !pq' = PQ.insert msg nodeTs newVal pq
    !newPQSize = pqsize + 1
    !newMaxPQSize = if newPQSize > maxpqsize then newPQSize else maxpqsize
    !newState = ReceiverState result newPQSize newMaxPQSize pq' mt' fc sk

{-# INLINE processNewMsg #-}
processNewMsg :: ReceiverState -> MinTimestampMap -> ValueMessage -> ReceiverState
processNewMsg state@(ReceiverState !result !pqsize !maxpqsize !pq _mt !fc !sk) !mt' msg@(ValueMessage !newVal !nodeTs) =
    case PQ.minView pq of
        Just (_, !nts, !val, !pq') ->
            if nts < nodeTs
                then
                    let !pq'' = PQ.insert msg nodeTs newVal pq'
                        !newResult = addValToResult result val
                        !newState = ReceiverState newResult pqsize maxpqsize pq'' mt' fc sk
                    in newState
                else
                    -- let !newResult = addValToResult result newVal
                    --     !newState = ReceiverState newResult pqsize maxpqsize pq mt' fc sk
                    -- in newState
                    let !newSk = sk + 1
                    in state { _skippedMsg = newSk, _minTimestamps = mt' }
        Nothing -> error "Empty queue!"

handleValueMessage :: Int -> ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage !msgBuffer state@(ReceiverState _ !pqsize _ _ !mt _ _) msg@(ValueMessage _ nodeTs@(NodeTimestamp _ !nodeId)) = do
    -- say $ "Received message: " <> show msg
    let mt' = HM.insert nodeId nodeTs mt
    return $ if pqsize < msgBuffer
        then
            let !newState = addNewMsgToPQueue state mt' msg
                !minNodeTs = minimum $ HM.elems mt'
            in reduceBufferForState minNodeTs newState
        else
            processNewMsg state mt' msg

handleFinishedMessage :: ReceiverState -> FinishedMessage -> Process ReceiverState
handleFinishedMessage !state Finished = do
    let !newFinishedCount = _finishedCount state + 1
        !newState = state { _finishedCount = newFinishedCount }
    return newState

calculateSumFromPQueue :: ValuePQueue -> Result -> Result
calculateSumFromPQueue !pq !partialResult = foldl' addValToResult partialResult vals
  where
    !vals = fmap (\(_, _, !v) -> v) $ sortBy (comparing (\(_, p, _) -> p)) $ PQ.toList pq -- TODO: optimize

whenStr :: Bool -> String -> String
whenStr p str = if p then str <> " " else ""

receiveWorkerLoop :: Timestamp -> Int -> Int -> ReceiverState -> Process ()
receiveWorkerLoop !showTime !nodesCount !msgBuffer !state = do
    !stateMay <- receiveTimeout 500 -- TODO: handle by message
        [ match $ handleValueMessage msgBuffer state
        , match $ handleFinishedMessage state
        ]
    now <- getCurrentTimeMicros
    let !state' = fromMaybe state stateMay
    if _finishedCount state' < nodesCount && now < showTime
        then receiveWorkerLoop showTime nodesCount msgBuffer state'
        else do
            let !finalResult = calculateSumFromPQueue (_valuePQueue state') (_partialResult state')
            let !info = whenStr (_finishedCount state' == nodesCount) "all-finished" <> whenStr (now >= showTime) "timeout"
                     <> "max-buffer: " <> show (_maxpqueueSize state') <> " " <> whenStr (_skippedMsg state' > 0) ("skipped: " <> show (_skippedMsg state'))
            say $ "Final result: " <> showResult finalResult <> " " <> info

receiveWorker :: Timestamp -> Int -> NodeId -> [NodeId] -> Process ()
receiveWorker !showTime !msgBuffer !masterNodeId !nodeIds = do
    registerSelf receiverService
    nsendRemote masterNodeId masterService Started
    receiveWorkerLoop showTime (length nodeIds) msgBuffer $ initialReceiverState nodeIds
