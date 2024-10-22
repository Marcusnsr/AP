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
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )
import Control.Monad (ap, forever, liftM, void)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

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
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
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
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
-- Messages sent to workers.
data WorkerMsg
  = MsgWorkerJob JobId Job
  | MsgWorkerTerminate

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  -- New messages for workers
  | MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | MsgWorkerIdle WorkerName
  | MsgJobDone JobId JobDoneReason WorkerName

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    -- New fields for workers
    spcWorkers :: [(WorkerName, Worker)],
    spcIdleWorkers :: [WorkerName]
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

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

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
  case (spcIdleWorkers state, spcJobsPending state) of
    (workerName : idleWorkers, (jobId, job) : pendingJobs) -> do
      let Just worker = lookup workerName (spcWorkers state)
      put $ state
        { spcIdleWorkers = idleWorkers,
          spcJobsPending = pendingJobs,
          spcJobsRunning = (jobId, job) : spcJobsRunning state
        }
      io $ sendToWorker worker (MsgWorkerJob jobId job)
      schedule  -- Continue scheduling
    _ -> return ()

-- | Helper function: Send a message to a worker.
sendToWorker :: Worker -> WorkerMsg -> IO ()
sendToWorker (Worker server) msg = sendTo server msg

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  modify $ \s -> s
    { spcJobsRunning = removeAssoc jobId (spcJobsRunning s),
      spcJobsDone = (jobId, reason) : spcJobsDone s
    }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = undefined

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

workerExists :: WorkerName -> SPCM Bool
workerExists = undefined

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
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
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    -- New case: Adding a worker
    MsgWorkerAdd workerName rsvp -> do
      state <- get
      if any ((== workerName) . fst) (spcWorkers state)
        then io $ reply rsvp $ Left $ "Worker " ++ workerName ++ " already exists."
        else do
          workerServer <- io $ spawn $ \workerChan -> workerProcess workerName c workerChan
          let worker = Worker workerServer
          modify $ \s -> s
            { spcWorkers = (workerName, worker) : spcWorkers s,
              spcIdleWorkers = workerName : spcIdleWorkers s
            }
          io $ reply rsvp $ Right worker
    -- New case: Worker becomes idle
    MsgWorkerIdle workerName -> do
      modify $ \s -> s { spcIdleWorkers = workerName : spcIdleWorkers s }
      schedule

    -- New case: Worker reports job completion
    MsgJobDone jobId reason workerName -> do
      modify $ \s -> s
        { spcJobsRunning = removeAssoc jobId (spcJobsRunning s),
          spcJobsDone = (jobId, reason) : spcJobsDone s
        }
      jobDone jobId reason
      modify $ \s -> s { spcIdleWorkers = workerName : spcIdleWorkers s }
      schedule
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid (spcJobsRunning state) of
        Just _ -> do
          let workerName = head [name | (name, Worker server) <- spcWorkers state, jobid `elem` map fst (spcJobsRunning state)]
          let Just worker = lookup workerName (spcWorkers state)
          io $ sendToWorker worker MsgWorkerTerminate
          modify $ \s -> s
            { spcJobsRunning = removeAssoc jobid (spcJobsRunning s),
              spcJobsDone = (jobid, DoneCancelled) : spcJobsDone s
            }
          jobDone jobid DoneCancelled
        Nothing -> return ()  -- Job is not running, nothing to cancel
    _ -> return ()  -- Handle unexpected messages

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkers = [],
            spcIdleWorkers = []
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workerName = requestReply c $ MsgWorkerAdd workerName

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop = undefined

workerProcess :: WorkerName -> Chan SPCMsg -> Chan WorkerMsg -> IO ()
workerProcess workerName spcChan workerChan = do
  -- Notify SPC that the worker is idle
  send spcChan (MsgWorkerIdle workerName)
  workerLoop
  where
    workerLoop = do
      msg <- receive workerChan
      case msg of
        MsgWorkerJob jobId job -> do
          -- Execute the job action
          jobAction job
          -- Notify SPC that the job is done
          send spcChan (MsgJobDone jobId Done workerName)
          -- Worker becomes idle again
          send spcChan (MsgWorkerIdle workerName)
          -- Continue looping
          workerLoop
        MsgWorkerTerminate -> return () -- Exit loop
