module Worker
    ( worker
    ) where

import           Protolude

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet as SL
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad.Loops
import           Data.Binary
import           Data.Binary.Orphans
import           Data.Hashable
import           Data.HashPSQ                                       (HashPSQ)
import qualified Data.HashPSQ                                       as PQ
import           Data.String                                        (String)
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Prelude                                            (error)
import           System.Random

import           Config

receiverService :: String
receiverService = "receiver"

type Timestamp = Integer
type ValuePQueue = HashPSQ ValueMessage Timestamp Double

data StatusMessage = Finished deriving (Show, Generic, Binary)

data ValueMessage = ValueMessage
    { nextValue :: Double
    , timestamp :: Timestamp
    } deriving (Show, Eq, Ord, Generic, Binary, Hashable)

data Result = Result
    { currentSum   :: Double
    , currentCount :: Integer
    } deriving (Show)

data ReceiverState = ReceiverState
    { partialResult   :: Result
    , latestTimeStamp :: Timestamp
    , pqueueSize      :: Int
    , messagePQueue   :: ValuePQueue
    , finishedCount   :: Int
    } deriving (Show)

data SenderState = SenderState
    { sentCounter :: Int
    , localSum    :: Double
    } deriving (Show)

maxPQSize :: Int
maxPQSize = 1

initialReceiverState :: ReceiverState
initialReceiverState = ReceiverState (Result 0.0 0) 0 0 PQ.empty 0

initialSenderState :: SenderState
initialSenderState = SenderState 0 0.0

showResult :: ReceiverState -> String
showResult = show
-- showResult state = "<" <> show (currentCount state) <> ", " <> show (currentSum state) <> ">"

timeResolution :: Int
timeResolution = 1000000

timeResolutionPOSIX :: POSIXTime
timeResolutionPOSIX = 1000000

getCurrentTimeMicros :: IO Timestamp
getCurrentTimeMicros = numerator . toRational . (* timeResolutionPOSIX) <$> getPOSIXTime

timeToShowResult :: Int
timeToShowResult = timeResolution `div` 10

-- Receiver ---------------------------------------------------------------

-- Test: ignoring the timestamp orders
handleValueMessageIgnoreTs :: ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessageIgnoreTs state@(ReceiverState (Result curSum count) lastTs pqsize pq fc) msg@(ValueMessage newVal newTs) = do
    say $ "Received message (old handler): " <> show msg
    return $ let newCount = count + 1 in ReceiverState (Result (curSum + fromIntegral newCount * newVal) newCount) newTs pqsize pq fc

-- Test: allow skipping messages arrived out of order
handleValueMessageSkipLateMsg :: ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessageSkipLateMsg state@(ReceiverState (Result curSum count) lastTs pqsize pq fc) msg@(ValueMessage newVal newTs) = do
    say $ "Received message (old handler): " <> show msg
    if lastTs < newTs -- TODO: this allows for skipping messages, idea: implement buffer
        then return $ let newCount = count + 1 in ReceiverState (Result (curSum + fromIntegral newCount * newVal) newCount) newTs pqsize pq fc
        else return state

handleValueMessage :: ReceiverState -> ValueMessage -> Process ReceiverState
handleValueMessage state@(ReceiverState (Result curSum count) lastTs pqsize pq fc) msg@(ValueMessage newVal newTs) = do
    say $ "Received message: " <> show msg
    -- add to priority queue
    if pqsize < maxPQSize
        then do
            say "Add to pq"
            let pq' = PQ.insert msg newTs newVal pq
            return $ ReceiverState (Result curSum count) newTs (pqsize + 1) pq' fc
        else
            case PQ.minView pq of
                Just (_, val, ts, pq') -> do
                    say $ "Remove from pq val " <> show newVal <> " ts " <> show newTs
                    let pq'' = PQ.insert msg newTs newVal pq'
                        newCount = count + 1
                    return $ ReceiverState (Result (curSum + fromIntegral newCount * newVal) newCount) newTs pqsize pq'' fc
                Nothing -> error "can't be here" -- return state

handleStatusMessage :: ReceiverState -> StatusMessage -> Process ReceiverState
handleStatusMessage state Finished = do
    -- say $ "Received Finished status, result: " <> showResult state
    let newFinishedCount = finishedCount state + 1
    return $ state { finishedCount = newFinishedCount }

calculateSumFromPQueue :: ValuePQueue -> Result -> Result
calculateSumFromPQueue pq partialResult = partialResult

receiveWorkerLoop :: Timestamp -> Int -> ReceiverState -> Process ()
receiveWorkerLoop showTime nodesCount state = do
    stateMay <- receiveTimeout 10
        [ match $ handleValueMessage state
        , match $ handleStatusMessage state
        ]
    now <- liftIO getCurrentTimeMicros
    -- if finishedCount state' == nodesCount || now >= showTime
    let state' = fromMaybe state stateMay
    if now < showTime
        then receiveWorkerLoop showTime nodesCount state'
        else do
            say $ "All nodes finished: " <> show (finishedCount state' == nodesCount) <> " Time is out: " <> show (now >= showTime)
               <> " Final result: " <> showResult state'
            -- state' <- case PQ.minView (messagePQueue state) of
            return ()

receiveWorker :: Timestamp -> Int -> Process ()
receiveWorker showTime nodesCount = do
    self <- getSelfPid
    -- say $ "ReceiveWorker pid: " <> show self
    register receiverService self
    receiveWorkerLoop showTime nodesCount initialReceiverState

-- Sender ---------------------------------------------------------------

sendNumbersLoop :: Timestamp -> StdGen -> [NodeId] -> SenderState -> Process ()
sendNumbersLoop stopTime gen nodeIds state@(SenderState count total) = do
    liftIO $ threadDelay $ 29 * 100000 -- TODO: for tests, remove later
    -- liftIO $ threadDelay $ 15 * 100000 -- TODO: for tests, remove later
    self <- getSelfPid
    now <- liftIO getCurrentTimeMicros
    let (val, gen') = randomR (0, 1) gen :: (Double, StdGen)
        msg = ValueMessage val now
        -- say $ show self <> " Random val: " <> show val <> " gen: " show gen'
    forM_ nodeIds $ \nodeId -> do
        say $ "Sending to node: " <> show nodeId <> " message: " <> show msg
        nsendRemote nodeId receiverService msg
    now <- liftIO getCurrentTimeMicros
    let state' = SenderState (count + 1) (total + val * fromIntegral count)
    if now < stopTime
        then sendNumbersLoop stopTime gen' nodeIds state'
        else say $ "Sender final state: " <> show state'

sendStop :: [NodeId] -> Process ()
sendStop nodeIds =
    forM_ nodeIds $ \nodeId ->
        -- say "Sending STOP"
        nsendRemote nodeId receiverService Finished

sendWorker :: Timestamp -> StdGen -> [NodeId] -> Process ()
sendWorker stopTime gen nodeIds = do
    -- self <- getSelfPid
    -- say $ "SendWorker pid: " <> show self
    sendNumbersLoop stopTime gen nodeIds initialSenderState
    sendStop nodeIds

-- Worker ---------------------------------------------------------------

worker :: WorkerConfig -> Process ()
worker workerConfig@(WorkerConfig config nodeIds) = do
    -- say $ "Worker config: " <> show workerConfig
    now <- liftIO getCurrentTimeMicros
    let sendDurationPrec = toInteger (timeResolution * sendDuration config)
        waitDurationPrec = toInteger (timeResolution * waitDuration config)
        showDurationPrec = waitDurationPrec - toInteger timeToShowResult
        stopTime = now      + sendDurationPrec
        showTime = stopTime + showDurationPrec
        killTime = stopTime + waitDurationPrec

    say $ "Current time: " <> show now
    say $ "Stop    time: " <> show stopTime
    say $ "Show    time: " <> show showTime
    say $ "Kill    time: " <> show killTime

    spawnLocal $ receiveWorker showTime $ length nodeIds

    let gen = mkStdGen $ seed config
    sendWorker stopTime gen nodeIds

    -- liftIO $ threadDelay 3000000
