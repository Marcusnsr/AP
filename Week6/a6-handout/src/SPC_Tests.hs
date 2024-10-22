module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "workerAdd success" $ do
          spc <- startSPC
          res <- workerAdd spc "worker1"
          case res of
            Right _ -> return ()
            Left err -> assertFailure $ "Expected success, got error: " ++ err

      , testCase "workerAdd duplicate" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          res <- workerAdd spc "worker1"
          case res of
            Left _ -> return ()  -- Expected an error
            Right _ -> assertFailure "Expected error due to duplicate worker name"

      , testCase "job execution" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          resultVar <- newIORef False
          let job = Job { jobAction = writeIORef resultVar True, jobMaxSeconds = 1 }
          _ <- jobAdd spc job
          threadDelay 1000  -- Wait for job to complete
          result <- readIORef resultVar
          result @?= True

      , testCase "job cancellation" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          resultVar <- newIORef False
          let job = Job { jobAction = writeIORef resultVar True, jobMaxSeconds = 1 }
          jobId <- jobAdd spc job
          jobCancel spc jobId
          threadDelay 1000
          result <- readIORef resultVar
          result @?= False
      ]
