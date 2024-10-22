module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10000000) $ -- Increased timeout to accommodate longer tests
    testGroup
      "SPC"
      [ testCase "simple-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "multiple-jobs" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          workerAdd spc "worker2"
          let num_jobs = 10
          ref <- newIORef (0 :: Int)
          js <- replicateM num_jobs $ jobAdd spc $ Job (modifyIORef ref (+ 1)) 1
          rs <- mapM (jobWait spc) js
          rs @?= replicate num_jobs (Just Done)
          v <- readIORef ref
          v @?= 10,
        testCase "duplicate-worker" $ do
          spc <- startSPC
          res1 <- workerAdd spc "worker1"
          res1 @?= Right (Worker "worker1")
          res2 <- workerAdd spc "worker1"
          case res2 of
            Left _ -> pure ()
            Right _ -> error "Expected failure when adding duplicate worker",
        testCase "no-workers" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobPending
          v <- readIORef ref
          v @?= False
          workerAdd spc "worker1"
          r2 <- jobWait spc j
          r2 @?= Just Done
          v2 <- readIORef ref
          v2 @?= True,
        -- New test cases for cancellation functionality
        testCase "cancel-pending-job" $ do
          spc <- startSPC
          -- No workers added, so the job remains pending
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobPending
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled
          v <- readIORef ref
          v @?= False, -- The job should not have run
        testCase "cancel-running-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          -- A job that takes some time to run
          j <- jobAdd spc $ Job (do threadDelay 2000000; writeIORef ref True) 3
          -- Wait a bit to ensure the job is running
          threadDelay 500000 -- 0.5 seconds
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled
          v <- readIORef ref
          v @?= False, -- The job should have been cancelled before writing to ref
        testCase "cancel-completed-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobWait spc j
          r1 @?= Just Done
          jobCancel spc j
          r2 <- jobStatus spc j
          r2 @?= Just (JobDone Done)
          v <- readIORef ref
          v @?= True, -- Job should have run
        testCase "cancel-already-cancelled-job" $ do
          spc <- startSPC
          -- No workers added, so the job remains pending
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          jobCancel spc j
          jobCancel spc j -- Cancel again
          r <- jobWait spc j
          r @?= Just DoneCancelled
          v <- readIORef ref
          v @?= False, -- Job should not have run
        testCase "cancel-multiple-jobs" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          workerAdd spc "worker2"
          ref <- newIORef (0 :: Int)
          let num_jobs = 5
          js <- replicateM num_jobs $ jobAdd spc $ Job (do threadDelay 2000000; modifyIORef ref (+1)) 3
          -- Wait a bit to ensure jobs are running
          threadDelay 100000
          -- Cancel all jobs
          mapM_ (jobCancel spc) js
          rs <- mapM (jobWait spc) js
          rs @?= replicate num_jobs (Just DoneCancelled)
          v <- readIORef ref
          v @?= 0, -- None of the jobs should have incremented ref
        testCase "cancel-job-after-completion" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobWait spc j
          r1 @?= Just Done
          jobCancel spc j -- Cancelling after completion should have no effect
          r2 <- jobStatus spc j
          r2 @?= Just (JobDone Done)
          v <- readIORef ref
          v @?= True,
        -- New test cases for timeout functionality
        testCase "job-times-out" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          -- Job that sleeps longer than its max allowed runtime
          j <- jobAdd spc $ Job (do threadDelay 3000000; writeIORef ref True) 1 -- Max 1 second
          r <- jobWait spc j
          r @?= Just DoneTimeout
          v <- readIORef ref
          v @?= False, -- The job should not have completed
        testCase "job-completes-before-timeout" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          -- Job that completes just before timeout
          j <- jobAdd spc $ Job (do threadDelay 500000; writeIORef ref True) 2 -- Max 2 seconds
          r <- jobWait spc j
          r @?= Just Done
          v <- readIORef ref
          v @?= True, -- The job should have completed
        testCase "multiple-jobs-with-timeouts" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          workerAdd spc "worker2"
          ref <- newIORef (0 :: Int)
          -- Jobs with varying runtimes and timeouts
          let jobs = [ (1000000, 2),  -- Sleep 1s, timeout 2s
                       (3000000, 1),  -- Sleep 3s, timeout 1s (should timeout)
                       (500000, 1),   -- Sleep 0.5s, timeout 1s
                       (4000000, 3) ] -- Sleep 4s, timeout 3s (should timeout)
          js <- mapM (\(sleepTime, maxTime) ->
                        jobAdd spc $ Job (do threadDelay sleepTime; modifyIORef ref (+1)) maxTime) jobs
          rs <- mapM (jobWait spc) js
          rs @?= [Just Done, Just DoneTimeout, Just Done, Just DoneTimeout]
          v <- readIORef ref
          v @?= 2, -- Only the jobs that didn't timeout incremented ref
        testCase "worker-handles-timeout-and-accepts-new-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref1 <- newIORef False
          ref2 <- newIORef False
          -- First job that will timeout
          j1 <- jobAdd spc $ Job (do threadDelay 3000000; writeIORef ref1 True) 1 -- Max 1 second
          r1 <- jobWait spc j1
          r1 @?= Just DoneTimeout
          v1 <- readIORef ref1
          v1 @?= False -- Job should not have completed
          -- Second job that should run normally
          j2 <- jobAdd spc $ Job (writeIORef ref2 True) 2
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v2 <- readIORef ref2
          v2 @?= True -- Job should have completed
        ]
