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


type ValuePQueue = HashPSQ ValueMessage NodeTimestamp Double
type MinTimestampMap = HashMap NodeId NodeTimestamp

data Result = Result
    { _currentSum   :: !Double
    , _currentCount :: !Integer
    } deriving (Show)

data ReceiverState = ReceiverState
    { _partialResult   :: !Result
    , _latestTimeStamp :: !Timestamp
    , _pqueueSize      :: !Int
    , _maxpqueueSize   :: !Int
    , _valuePQueue     :: !ValuePQueue
    , _minTimestamps   :: !MinTimestampMap
    , _finishedCount   :: !Int
    , _skippedMsg      :: !Integer
    } deriving (Show)

-- IDEAS:
-- - timer message

initialReceiverState :: ReceiverState
initialReceiverState = ReceiverState (Result 0.0 0) 0 0 0 PQ.empty HM.empty 0 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

-- reduceBuffer :: NodeTimestamp -> (Result, ValuePQueue) -> (Result, ValuePQueue)
-- reduceBuffer minNodeTs resultPQueue@(Result curSum count, pq) = newResultQueue
--   where
--     newResultQueue = case PQ.minView pq of
--         Just (_, nts@(NodeTimestamp _ts _nid), val, pq') ->
--             if nts < minNodeTs
--                 then
--                     let !newCount = count + 1
--                         !newSum = curSum + fromIntegral newCount * val
--                         !newResult = Result newSum newCount
--                     in (newResult, pq')
--                 else resultPQueue
--         Nothing -> resultPQueue

reduceBuffer :: NodeTimestamp -> (Result, ValuePQueue) -> Process (Result, ValuePQueue)
reduceBuffer minNodeTs resultPQueue@(Result curSum count, pq) =
    case PQ.minView pq of
        Just (_, nts@(NodeTimestamp _ts _nid), val, pq') -> do
            -- say $ "nts " <> show nts <> " val " <> show val <> " minNodeTs " <> show minNodeTs
            if nts < minNodeTs -- && False
                then do
                    -- say $ "don't skip " -- <> show nts
                    let !newCount = count + 1
                        !newSum = curSum + fromIntegral newCount * val
                        !newResult = Result newSum newCount
                    return (newResult, pq')
                else
                    -- say $ "skip " -- <> show nts
                    return resultPQueue
        -- Nothing -> return resultPQueue
        Nothing -> error "can't be here"


-- reduceBufferForState :: NodeTimestamp -> ReceiverState -> ReceiverState
-- reduceBufferForState minNodeTs _state@(ReceiverState result@(Result !_curSum !count) lastTs !pqsize !maxpqsize !pq !mt !fc !sk) = state'
--   where
--     (!newResult@(Result _ count'), !pq') = reduceBuffer minNodeTs (result, pq)
--     reducedNum = count' - count
--     newPQSize = pqsize - fromIntegral reducedNum
--     !state' = ReceiverState newResult lastTs newPQSize maxpqsize pq' mt fc sk
reduceBufferForState :: NodeTimestamp -> ReceiverState -> Process ReceiverState
reduceBufferForState minNodeTs _state@(ReceiverState result@(Result !_curSum !count) lastTs !pqsize !maxpqsize !pq !mt !fc !sk) = if pqsize == 0
    then return _state
    else do
        (!newResult@(Result _ count'), !pq') <- reduceBuffer minNodeTs (result, pq)
        let reducedNum = count' - count
        -- let (!newResult@(Result _ count'), !pq') = reduceBuffer minNodeTs (result, pq)
        --     reducedNum = count' - count
            newPQSize = pqsize - fromIntegral reducedNum
            !state' = ReceiverState newResult lastTs newPQSize maxpqsize pq' mt fc sk
        -- say $ "minNodeTs " <> show minNodeTs
        -- say $ "(reduce ______from " <> show minNodeTs <> " " <> show (result, "pq") <> ") _____to (" <> show (newResult, "pq'") <> ")"
        -- say $ "(red __from " <> showResult result <> ") __to (" <> showResult newResult <> ")" <> " pqsize " <> show pqsize
        -- say $ "(reduce ______from " <> show (result, "pq") <> ") _____to (" <> show (newResult, "pq'") <> ")"
        -- say $ "(reducedNum " <> show reducedNum <> ") (c' " <> show count' <> ") (c " <> show count <> ") (newPQSizeGood " <> show (newPQSize == PQ.size pq')
        --    <> ") (oldPQSize " <> show pqsize <> ")"

        return state'


handleValueMessage :: Int -> Int -> ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage !msgBuffer !nodesCount state@(ReceiverState result@(Result !curSum !count) _lastTs !pqsize !maxpqsize !pq !mt !fc !sk) msg@(ValueMessage newVal nodeTs@(NodeTimestamp newTs nodeId)) = do
    -- say $ "Received message: " <> show msg
    let newNodeMinTsMap = HM.insert nodeId nodeTs mt
    if pqsize < msgBuffer
        then do
            -- say "Add to pq"
            let !pq' = PQ.insert msg nodeTs newVal pq
                !newPQSize = pqsize + 1
                !newMaxPQSize = if newPQSize > maxpqsize then newPQSize else maxpqsize
                !newState = ReceiverState result newTs newPQSize newMaxPQSize pq' newNodeMinTsMap fc sk
                !minNodeTs = minimum $ HM.elems newNodeMinTsMap
                -- !newState' = reduceBufferForState minNodeTs newState
            -- say $ "new minimum " <> show minNodeTs
            -- !newState' <- reduceBufferForState minNodeTs newState
            if HM.size newNodeMinTsMap < nodesCount
                then return newState
                -- else return newState
                else reduceBufferForState minNodeTs newState
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
                                !newState = ReceiverState newResult newTs pqsize maxpqsize pq'' newNodeMinTsMap fc sk
                            return newState
                        else do -- skip message :(
                            -- say "message skipped"
                            let !newSk = sk + 1
                            return $ state { _skippedMsg = newSk, _minTimestamps = newNodeMinTsMap }
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
    !pl = PQ.toList pq -- order not specified
    !spl = sortBy (comparing (\(nt, p, v) -> p)) pl
    !result = foldl' calcSum partialResult spl -- TODO: check correctness
    calcSum (Result !s !m) (_, _, !v) = let !newM = m + 1 in Result (s + fromInteger newM * v) newM
    -- !result = foldr calcSum partialResult pl -- TODO: check correctness
    -- calcSum (_, _, !v) (Result !s !m) = Result (s + v) (m + 1)

whenStr :: Bool -> String -> String
whenStr p str = if p then str <> " " else ""

receiveWorkerLoop :: Timestamp -> Int -> Int -> ReceiverState -> Process ()
receiveWorkerLoop !showTime !nodesCount !msgBuffer !state = do
    !stateMay <- receiveTimeout 500 -- TODO: handle by message
        [ match $ handleValueMessage msgBuffer nodesCount state
        , match $ handleFinishedMessage state
        ]
    -- timer message
    -- 0.155+2*0.77+3*0.396+4*0.697=5.67
    -- 0.7704+2*0.1551+3*0.396+4*0.69738=5.058
    now <- liftIO getCurrentTimeMicros
    let !state' = fromMaybe state stateMay
    if _finishedCount state' < nodesCount && now < showTime
        then receiveWorkerLoop showTime nodesCount msgBuffer state'
        else do
            -- say $ "queue " <> show (PQ.toList (_valuePQueue state'))
            -- say $ "res " <> show (_partialResult state') <> " queue " <> show (fmap (\(nt, p, v) -> v) $ sortBy (comparing (\(nt, p, v) -> p)) $ PQ.toList (_valuePQueue state'))
            -- say $ "res " <> show (_partialResult state') <> " queue full " <> show (sortBy (comparing (\(nt, p, v) -> nt)) $ PQ.toList (_valuePQueue state'))
            -- calcStart <- liftIO getCurrentTimeMicros
            let !finalResult = calculateSumFromPQueue (_valuePQueue state') (_partialResult state')
            -- calcEnd <- liftIO getCurrentTimeMicros
            let !info = whenStr (_finishedCount state' == nodesCount) "all-finished" <> whenStr (now >= showTime) "timeout"
                     <> "max-buffer: " <> show (_maxpqueueSize state') <> " " <> whenStr (_skippedMsg state' > 0) ("skipped: " <> show (_skippedMsg state'))
                    --  <> " result time " <> show (calcEnd - calcStart)
            say $ "Final result: " <> showResult finalResult <> " " <> info
            -- say $ "Final map: " <> show (_minTimestamps state')
            -- say $ "Final result: " <> showResult finalResult
            -- say $ "Final re: " <> show (_minTimestamps state')

receiveWorker :: Timestamp -> Int -> NodeId -> [NodeId] -> Process ()
receiveWorker !showTime !msgBuffer !masterNodeId !nodeIds = do
    -- self <- getSelfPid
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService =<< getSelfPid

    nsendRemote masterNodeId masterService Started

    receiveWorkerLoop showTime (length nodeIds) msgBuffer initialReceiverState
