module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10000000) $
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
          v @?= False, 
        testCase "cancel-running-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (do threadDelay 2000000; writeIORef ref True) 3
          threadDelay 500000
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled
          v <- readIORef ref
          v @?= False,
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
          v @?= True, 
        testCase "cancel-already-cancelled-job" $ do
          spc <- startSPC
          -- No workers added, so the job remains pending
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          jobCancel spc j
          jobCancel spc j 
          r <- jobWait spc j
          r @?= Just DoneCancelled
          v <- readIORef ref
          v @?= False, 
        testCase "cancel-multiple-jobs" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          workerAdd spc "worker2"
          ref <- newIORef (0 :: Int)
          let num_jobs = 5
          js <- replicateM num_jobs $ jobAdd spc $ Job (do threadDelay 2000000; modifyIORef ref (+1)) 3
          threadDelay 100000
          -- Cancel all jobs
          mapM_ (jobCancel spc) js
          rs <- mapM (jobWait spc) js
          rs @?= replicate num_jobs (Just DoneCancelled)
          v <- readIORef ref
          v @?= 0,
        testCase "cancel-job-after-completion" $ do
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
          v @?= True,
        -- test cases for timeout functionality
        testCase "job-times-out" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (do threadDelay 3000000; writeIORef ref True) 1
          r <- jobWait spc j
          r @?= Just DoneTimeout
          v <- readIORef ref
          v @?= False,
        testCase "job-completes-before-timeout" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (do threadDelay 500000; writeIORef ref True) 2 
          r <- jobWait spc j
          r @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "multiple-jobs-with-timeouts" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          workerAdd spc "worker2"
          ref <- newIORef (0 :: Int)
          let jobs = [ (1000000, 2),
                       (3000000, 1),
                       (500000, 1),
                       (4000000, 3) ]
          js <- mapM (\(sleepTime, maxTime) ->
                        jobAdd spc $ Job (do threadDelay sleepTime; modifyIORef ref (+1)) maxTime) jobs
          rs <- mapM (jobWait spc) js
          rs @?= [Just Done, Just DoneTimeout, Just Done, Just DoneTimeout]
          v <- readIORef ref
          v @?= 2, 
        testCase "worker-handles-timeout-and-accepts-new-job" $ do
          spc <- startSPC
          workerAdd spc "worker1"
          ref1 <- newIORef False
          ref2 <- newIORef False
          j1 <- jobAdd spc $ Job (do threadDelay 3000000; writeIORef ref1 True) 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneTimeout
          v1 <- readIORef ref1
          v1 @?= False
          j2 <- jobAdd spc $ Job (writeIORef ref2 True) 2
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v2 <- readIORef ref2
          v2 @?= True
        ]
