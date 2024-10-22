module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    Worker (..),
    workerAdd,
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    SomeException,
    catch,
    fromException,
  )
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (find, partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

-- | A name for a worker.
type WorkerName = String

-- | A handle to a worker.
data Worker = Worker WorkerName
  deriving (Eq, Show)

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | -- | Some time has passed.
    MsgTick
  | -- | Add a worker.
    MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | -- | Worker reports job is done.
    MsgWorkerDone WorkerName JobId JobDoneReason

-- Messages sent to Workers.
data WorkerMsg
  = -- | Assign a job to the worker.
    MsgDoJob JobId Job
  | -- | Cancel the job with the given JobId and reason.
    MsgCancelJob JobId JobDoneReason

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state.
data SPCState = SPCState
  { spcChan :: Chan SPCMsg,
    spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [RunningJob],
    spcJobsDone :: [(JobId, JobDoneReason)],
    -- | These are waiting for this job to terminate.
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, WorkerStatus, Server WorkerMsg)]
  }

-- | The status of a worker.
data WorkerStatus
  = Idle
  | Busy JobId

-- | Information about a running job.
data RunningJob = RunningJob
  { rjJobId :: JobId,
    rjWorkerName :: WorkerName,
    rjStartTime :: Seconds,
    rjMaxSeconds :: Int
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (findIdleWorker (spcWorkers state), spcJobsPending state) of
    (Just (wname, wserver), (jobid, job) : jobs) -> do
      -- Send job to worker
      io $ sendTo wserver $ MsgDoJob jobid job
      -- Update worker status to Busy
      let workers' = updateWorkerStatus wname (Busy jobid) (spcWorkers state)
      -- Update jobs running
      now <- io getSeconds
      let runningJob = RunningJob jobid wname now (jobMaxSeconds job)
      let jobsRunning' = runningJob : spcJobsRunning state
      put $
        state
          { spcWorkers = workers',
            spcJobsPending = jobs,
            spcJobsRunning = jobsRunning'
          }
    _ -> pure ()

-- Helper functions for worker management
findIdleWorker :: [(WorkerName, WorkerStatus, Server WorkerMsg)] -> Maybe (WorkerName, Server WorkerMsg)
findIdleWorker [] = Nothing
findIdleWorker ((wname, Idle, wserver) : ws) = Just (wname, wserver)
findIdleWorker (_ : ws) = findIdleWorker ws

updateWorkerStatus :: WorkerName -> WorkerStatus -> [(WorkerName, WorkerStatus, Server WorkerMsg)] -> [(WorkerName, WorkerStatus, Server WorkerMsg)]
updateWorkerStatus wname newStatus [] = []
updateWorkerStatus wname newStatus ((wn, ws, srv) : rest)
  | wname == wn = (wn, newStatus, srv) : rest
  | otherwise = (wn, ws, srv) : updateWorkerStatus wname newStatus rest

lookupWorker :: WorkerName -> [(WorkerName, WorkerStatus, Server WorkerMsg)] -> Maybe (WorkerStatus, Server WorkerMsg)
lookupWorker wname [] = Nothing
lookupWorker wname ((wn, ws, srv) : rest)
  | wname == wn = Just (ws, srv)
  | otherwise = lookupWorker wname rest

removeRunningJob :: JobId -> [RunningJob] -> [RunningJob]
removeRunningJob jid = filter (\rj -> rjJobId rj /= jid)

-- Precondition: 'jobid' is currently running.
jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      let jobsRunning' = removeRunningJob jobid $ spcJobsRunning state
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobsRunning = jobsRunning'
          }

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  let (timedOutJobs, stillRunningJobs) = partition (\rj -> now - rjStartTime rj >= fromIntegral (rjMaxSeconds rj)) (spcJobsRunning state)
  forM_ timedOutJobs $ \rj -> do
    -- Send MsgCancelJob to the worker
    let wname = rjWorkerName rj
    case lookupWorker wname (spcWorkers state) of
      Just (_, wserver) -> io $ sendTo wserver $ MsgCancelJob (rjJobId rj) DoneTimeout
      Nothing -> pure () -- Worker not found
  -- Update spcJobsRunning
  put $ state { spcJobsRunning = stillRunningJobs }

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $
        reply rsvp $
          case
            ( lookup jobid $ spcJobsPending state,
              find (\rj -> rjJobId rj == jobid) $ spcJobsRunning state,
              lookup jobid $ spcJobsDone state
            ) of
            (Just _, _, _) -> Just JobPending
            (_, Just _, _) -> Just JobRunning
            (_, _, Just r) -> Just $ JobDone r
            _ -> Nothing
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}
    MsgJobCancel cancel_jobid -> do
      state <- get
      case lookup cancel_jobid $ spcJobsPending state of
        Just _ -> do
          -- Job is pending, remove it from pending list
          let jobsPending' = removeAssoc cancel_jobid $ spcJobsPending state
          put $ state {spcJobsPending = jobsPending'}
          jobDone cancel_jobid DoneCancelled
        Nothing ->
          case find (\rj -> rjJobId rj == cancel_jobid) $ spcJobsRunning state of
            Just rj -> do
              -- Job is running, send cancellation to worker
              let wname = rjWorkerName rj
              case lookupWorker wname $ spcWorkers state of
                Just (_, wserver) -> do
                  io $ sendTo wserver $ MsgCancelJob cancel_jobid DoneCancelled
                Nothing -> pure () -- Worker not found
            Nothing ->
              -- Job is not found, may have already completed
              pure ()
    MsgWorkerDone wname jobid reason -> do
      state <- get
      let workers' = updateWorkerStatus wname Idle (spcWorkers state)
      put $ state {spcWorkers = workers'}
      jobDone jobid reason
    MsgTick -> do
      checkTimeouts
    MsgWorkerAdd wname rsvp -> do
      state <- get
      case lookupWorker wname (spcWorkers state) of
        Just _ -> do
          io $ reply rsvp $ Left $ "Worker with name " ++ wname ++ " already exists."
        Nothing -> do
          workerServer <- io $ spawn (workerProcess (spcChan state) wname Nothing)
          let worker = Worker wname
          let workers' = (wname, Idle, workerServer) : spcWorkers state
          put $ state {spcWorkers = workers'}
          io $ reply rsvp $ Right worker

workerProcess :: Chan SPCMsg -> WorkerName -> Maybe (JobId, ThreadId, MVar JobDoneReason) -> Chan WorkerMsg -> IO ()
workerProcess spcChan wname mRunningJob workerChan = do
  msg <- receive workerChan
  case msg of
    MsgDoJob jobid job -> do
      cancellationReasonVar <- newEmptyMVar
      tid <- forkIO $ do
        let doJob = do
              jobAction job
              send spcChan $ MsgWorkerDone wname jobid Done
            onException :: SomeException -> IO ()
            onException e = do
              mReason <- tryTakeMVar cancellationReasonVar
              case mReason of
                Just reason -> send spcChan $ MsgWorkerDone wname jobid reason
                Nothing -> case fromException e of
                  Just ThreadKilled -> send spcChan $ MsgWorkerDone wname jobid DoneCancelled
                  _ -> send spcChan $ MsgWorkerDone wname jobid DoneCrashed
        doJob `catch` onException
      workerProcess spcChan wname (Just (jobid, tid, cancellationReasonVar)) workerChan
    MsgCancelJob jobid reason -> do
      case mRunningJob of
        Just (runningJobId, tid, cancellationReasonVar) | runningJobId == jobid -> do
          putMVar cancellationReasonVar reason
          killThread tid
          workerProcess spcChan wname Nothing workerChan
        _ -> workerProcess spcChan wname mRunningJob workerChan

startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWaiting = [],
            spcWorkers = [],
            spcChan = c
          }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer server
  pure $ SPC server
  where
    timer server _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo server MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Query the job status. Returns 'Nothing' if job is not known to
-- this SPC instance.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a worker to SPC.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) wname =
  requestReply c $ MsgWorkerAdd wname
