{-# LANGUAGE BangPatterns #-}

module Node.Receiver
    ( receiveWorker
    ) where

import           Protolude                   hiding (state)

import           Control.Distributed.Process (NodeId (..), Process, match, nsendRemote, receiveTimeout, say)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.HashPSQ                (HashPSQ)
import qualified Data.HashPSQ                as PQ
import           Data.String                 (String)
import           Network.Transport           (EndPointAddress (..))
import           Prelude                     (error)

import           Node.Common

type ValuePQueue     = HashPSQ ValueMessage NodeTimestamp Double

type MinTimestampMap = HashMap NodeId NodeTimestamp

data Result = Result
    { _currentSum          :: !Double
    , _currentCount        :: !Integer
    , _latestNodeTimestamp :: !NodeTimestamp
    } deriving (Show)

data ReceiverState = ReceiverState
    { _partialResult :: !Result
    , _pqueueSize    :: !Int
    , _maxpqueueSize :: !Int
    , _valuePQueue   :: !ValuePQueue
    , _minTimestamps :: !MinTimestampMap
    , _finishedCount :: !Int
    , _discardedMsg  :: !Integer
    } deriving (Show)

initialMinTimestampMap :: [NodeId] -> MinTimestampMap
initialMinTimestampMap nodeIds = HM.fromList $ initWithZeroTimestamp <$> nodeIds
  where
    initWithZeroTimestamp nodeId = (nodeId, NodeTimestamp 0 nodeId)

initialNodeTimestamp :: NodeTimestamp
initialNodeTimestamp = NodeTimestamp 0 (NodeId (EndPointAddress ""))

initialReceiverState :: [NodeId] -> ReceiverState
initialReceiverState nodeIds = ReceiverState (Result 0.0 0 initialNodeTimestamp) 0 0 PQ.empty (initialMinTimestampMap nodeIds) 0 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

reduceBuffer :: NodeTimestamp -> (Result, ValuePQueue) -> (Result, ValuePQueue)
reduceBuffer minNodeTs resultPQueue@(!result, !pq) =
    case PQ.minView pq of
        Just (_, nts, val, pq') ->
            if nts <= minNodeTs
                then reduceBuffer minNodeTs (addValToResult result val nts, pq')
                else resultPQueue
        Nothing -> resultPQueue

{-# INLINE reduceBufferForState #-}
reduceBufferForState :: NodeTimestamp -> ReceiverState -> ReceiverState
reduceBufferForState minNodeTs state@(ReceiverState result@(Result _ !count _) !pqsize !maxpqsize !pq !mt !fc !sk)
    | pqsize == 0 = state
    | otherwise =
        let (!result'@(Result _ count' _), !pq') = reduceBuffer minNodeTs (result, pq)
            !pqsize' = pqsize - fromIntegral (count' - count)
        in ReceiverState result' pqsize' maxpqsize pq' mt fc sk

{-# INLINE addValToResult #-}
addValToResult :: Result -> Double -> NodeTimestamp -> Result
addValToResult (Result !s !m _) !val !newNodeTs = let !m' = m + 1 in Result (s + fromInteger m' * val) m' newNodeTs

{-# INLINE addNewMsgToPQueue #-}
addNewMsgToPQueue :: ReceiverState -> ValueMessage -> ReceiverState
addNewMsgToPQueue (ReceiverState !result !pqsize !maxpqsize !pq !mt !fc !sk) msg@(ValueMessage !newVal !nodeTs) = state'
  where
    !pq'        = PQ.insert msg nodeTs newVal pq
    !pqsize'    = pqsize + 1
    !maxpqsize' = if pqsize' > maxpqsize then pqsize' else maxpqsize
    !state'     = ReceiverState result pqsize' maxpqsize' pq' mt fc sk

{-# INLINE discardMessage #-}
discardMessage :: ReceiverState -> ReceiverState
discardMessage state = let !sk' = _discardedMsg state + 1 in state { _discardedMsg = sk' }

{-# INLINE processNewMsg #-}
processNewMsg :: ReceiverState -> NodeTimestamp -> ValueMessage -> ReceiverState
processNewMsg state@(ReceiverState result@(Result _ _ !latestNodeTs) !pqsize !maxpqsize !pq !mt !fc !sk) minNodeTs msg@(ValueMessage !newVal !newNodeTs)
    | newNodeTs < latestNodeTs = discardMessage state
    | otherwise =
        case PQ.minView pq of
            Just (_, !nts, !val, !pq') ->
                if nts < newNodeTs || minNodeTs < newNodeTs
                    then
                        let !pq''    = PQ.insert msg newNodeTs newVal pq'
                            !result' = addValToResult result val nts
                        in ReceiverState result' pqsize maxpqsize pq'' mt fc sk
                    else discardMessage state
            Nothing -> error "Empty queue!"

handleValueMessage :: Int -> ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage !maxBufferSize state@(ReceiverState _ !pqsize _ _ !mt _ _) msg@(ValueMessage _ newNodeTs@(NodeTimestamp _ !nodeId)) = do
    let !mt'      = HM.insert nodeId newNodeTs mt
        !state'   = state { _minTimestamps = mt' }
        minNodeTs = minimum $ HM.elems mt'
    return $ if pqsize < maxBufferSize
        then
            let !state'' = addNewMsgToPQueue state' msg
            in reduceBufferForState minNodeTs state''
        else
            processNewMsg state' minNodeTs msg

handleFinishedMessage :: ReceiverState -> FinishedMessage -> Process ReceiverState
handleFinishedMessage !state Finished = do
    let !finishedCount' = _finishedCount state + 1
    return $ state { _finishedCount = finishedCount' }

calculateSumFromPQueue :: ValuePQueue -> Result -> Result
calculateSumFromPQueue !pq !partialResult = foldl' addNewVal partialResult vals
  where
    !vals = fmap (\(_, !nts, !v) -> (v, nts)) $ sortBy (comparing (\(_, nts, _) -> nts)) $ PQ.toList pq -- TODO: optimize
    addNewVal result (val, newNodeTs) = addValToResult result val newNodeTs


calcResult :: Timestamp -> Timestamp -> Int -> ReceiverState -> Process ()
calcResult !now !showTime !nodesCount !state = do
    let !finalResult = calculateSumFromPQueue (_valuePQueue state) (_partialResult state)
    let info = whenStr (_finishedCount state == nodesCount) "all-finished"
            <> whenStr (now >= showTime) "timeout"
            <> "max-buffer: " <> show (_maxpqueueSize state) <> " "
            <> whenStr (_discardedMsg state > 0) ("discarded: " <> show (_discardedMsg state))
        whenStr p str = if p then str <> " " else ""
    say $ "Final result: " <> showResult finalResult <> " " <> info

receiveWorkerLoop :: Timestamp -> Int -> Int -> ReceiverState -> Process ()
receiveWorkerLoop !showTime !nodesCount !maxBufferSize !state = do
    stateMay <- receiveTimeout 50000
        [ match $ handleValueMessage maxBufferSize state
        , match $ handleFinishedMessage state
        ]
    let !state' = fromMaybe state stateMay
    now <- getCurrentTimeMicros
    if _finishedCount state' < nodesCount && now < showTime
        then receiveWorkerLoop showTime nodesCount maxBufferSize state'
        else calcResult now showTime nodesCount state'

receiveWorker :: Timestamp -> Int -> NodeId -> [NodeId] -> Process ()
receiveWorker !showTime !maxBufferSize !masterNodeId !nodeIds = do
    registerSelf receiverService
    nsendRemote masterNodeId masterService Started
    receiveWorkerLoop showTime (length nodeIds) maxBufferSize $ initialReceiverState nodeIds
