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

data SenderState = SenderState
    { _sentCounter :: Int
    , _localSum    :: Double
    } deriving (Show)

maxPQSize :: Int
maxPQSize = 100

initialReceiverState :: ReceiverState
initialReceiverState = ReceiverState (Result 0.0 0) 0 0 PQ.empty 0

showResult :: Result -> String
showResult result = "<" <> show (_currentCount result) <> ", " <> show (_currentSum result) <> ">"

handleValueMessage :: ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage (ReceiverState (Result curSum count) _lastTs pqsize pq fc) msg@(ValueMessage newVal newTs) =
    -- say $ "Received message: " <> show msg
    if pqsize < maxPQSize
        then do
            -- say "Add to pq"
            let pq' = PQ.insert msg newTs newVal pq
            return $ ReceiverState (Result curSum count) newTs (pqsize + 1) pq' fc
        else
            case PQ.minView pq of
                Just (_, _ts, val, pq') -> do
                    -- say $ "Remove from pq val " <> show val <> " ts " <> show ts
                    let pq'' = PQ.insert msg newTs newVal pq'
                        newCount = count + 1
                    return $ ReceiverState (Result (curSum + fromIntegral newCount * val) newCount) newTs pqsize pq'' fc
                Nothing -> error "can't be here" -- return state

handleStatusMessage :: ReceiverState -> StatusMessage -> Process ReceiverState
handleStatusMessage state Finished = do
    -- say $ "Received Finished status, result: " <> showResult state
    let newFinishedCount = _finishedCount state + 1
    return $ state { _finishedCount = newFinishedCount }

calculateSumFromPQueue :: ValuePQueue -> Result -> Result
calculateSumFromPQueue pq partialResult = result
  where
    pl = PQ.toList pq
    result = foldl' calcSum partialResult pl
    calcSum (Result s m) (_, _, v) = Result (s + v) (m + 1)

receiveWorkerLoop :: Timestamp -> Int -> ReceiverState -> Process ()
receiveWorkerLoop showTime nodesCount state = do
    stateMay <- receiveTimeout 10
        [ match $ handleValueMessage state
        , match $ handleStatusMessage state
        ]
    now <- liftIO getCurrentTimeMicros
    let state' = fromMaybe state stateMay
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
