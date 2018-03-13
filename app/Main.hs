{-# LANGUAGE RankNTypes #-}
module Main where

import Lib
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded as UB
import           Control.Monad
import           Data.IntSet                           as Set
import           Data.IORef
import           Data.List.NonEmpty                    as NE
import           Data.Monoid
import           Debug.Trace
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random.MWC
import           System.Timeout

main :: IO ()
main = testRun
  -- args <- getArgs
  -- case args of
  --   [giveUp, chanSize, iterationsS] -> do
  --     let iterations = read iterationsS
  --     seen <- fuzzTest (read giveUp) (read chanSize) iterations safeAlgo
  --     case countMismatches iterations seen of
  --       0 -> putStrLn "Success!"
  --       mismatches -> do
  --         bail ("Found " <> show mismatches <> " mismatches")
  --   _ -> usage
  -- where
  --   usage = bail "ARGS: giveUp (microseconds) chanSize iterations, e.g. 500000 10000 100"
  --   bail msg = do
  --     hPutStrLn stderr msg
  --     exitFailure


-- | Blockingly reads the chan
simpleAlgo :: InChan a -> OutChan a -> IO a
simpleAlgo _ = UB.readChan


--FIXME: this may be a non-starter, we cannot put what we got back on
--the front of the queue. TBMQueue supports unGet but we don't need
--that strong of a guarantee in the implementation because its fine if
--the logs get sent slightly out of order.

-- | When interrupted, tries to un-take the value
safeAlgo :: InChan a  -> OutChan a -> IO a
safeAlgo inChan outChan = UB.readChanOnException outChan tryGiveBack
  where
    tryGiveBack getDropped = do
      -- putStrLn "readChan got interrupted. Data would be lost with readChan"
      -- uhh, somehow with a relatively high giveUp, 500ms, this just keeps trucking. when I'd expect it would put this on the end of the queue and get a mismatch. For lower delays like 50ms it falls on its face
      void . tryWriteChan inChan =<< getDropped
      -- -- uh does this only work if i call getDropped?
      -- _ <- getDropped
      -- preempted


-- fuzzTest
--   :: Int
--   -- ^ Give up timeout in loop
--   -> Int
--   -- ^ Chan size, set it quite high
--   -> Int
--   -- ^ Iterations
--   -> (forall a. IO () -> InChan a -> OutChan a -> IO a)
--   -> IO Result
-- fuzzTestEarlyTermination giveUp chanSize iterations algo = do
--   gen <- createSystemRandom
--   (inChan, outChan) <- UB.newChan chanSize
--   writer <- async (writeLoop gen inChan 0)
--   reader <- async (readerLoop inChan outChan 0)
--   link2 writer reader
--   res <- wait reader
--   cancel writer
--   return res
--   where
--     writeLoop gen inChan n
--       | n > iterations = return ()
--       | otherwise = do
--           let minWait = ceiling (fromIntegral giveUp * 0.9)
--           let maxWait = ceiling (fromIntegral giveUp * 1.1)
--           threadDelay =<< uniformR (minWait, maxWait) gen
--           UB.writeChan inChan n
--           -- putStrLn ("Wrote " <> show n)
--           writeLoop gen inChan (n + 1)
--           --FIXME: seems like once we give up, we give up forever
--     readerLoop inChan outChan expected
--       | expected > iterations = return OK
--       | otherwise = do
--           preempted <- newIORef False
--           let signalPreempted = writeIORef preempted True
--           res <- race (threadDelay giveUp) (algo signalPreempted inChan outChan)
--           let proceed = readerLoop inChan outChan (expected + 1)
--           case res of
--             Left _ -> do
--               wasPreempted <- readIORef preempted
--               if wasPreempted
--                  then do
--                    putStrLn "Preempted. Skipping. IRL we'd put the value on the end of the queue"
--                    proceed
--                  else do
--                    putStrLn "Gave up. Trying again"
--                    readerLoop inChan outChan expected
--             Right actual
--               | actual == expected -> do
--                   putStrLn "gotem"
--                   proceed
--               | otherwise -> do
--                   -- actually this never happens. on the safe version of readChan it seems like it blocks until it gets the *next* value, so it recovers from an async exception?
--                   wasPreempted <- readIORef preempted
--                   if wasPreempted
--                      then do
--                        putStrLn "Preempted. Skipping. IRL we'd put the value on the end of the queue"
--                        proceed
--                      else do
--                        return (Mismatch expected actual)


-- data Result = Mismatch Int Int | OK


fuzzTest
  :: Int
  -- ^ Give up timeout in loop
  -> Int
  -- ^ Chan size, set it quite high
  -> Int
  -- ^ Iterations
  -> (forall a. InChan a -> OutChan a -> IO a)
  -> IO IntSet
fuzzTest giveUp chanSize iterations algo = do
  gen <- createSystemRandom
  (inChan, outChan) <- UB.newChan chanSize
  writerDoneRef <- newIORef False
  writer <- async (writeLoop gen inChan 1)
  reader <- async (readerLoop writerDoneRef inChan outChan mempty)
  link2 writer reader
  wait writer
  writeIORef writerDoneRef True
  res <- wait reader
  return res
  where
    writeLoop gen inChan n
      | n > iterations = return ()
      | otherwise = do
          let minWait = ceiling (fromIntegral giveUp * 0.9)
          let maxWait = ceiling (fromIntegral giveUp * 1.1)
          threadDelay =<< uniformR (minWait, maxWait) gen
          UB.writeChan inChan n
          -- putStrLn ("Wrote " <> show n)
          writeLoop gen inChan (n + 1)
          --FIXME: seems like once we give up, we give up forever
    readerLoop writerDoneRef inChan outChan seen = do
      res <- race (threadDelay giveUp) (algo inChan outChan)
      case res of
        Left _ -> do
          -- putStrLn "Gave up. Trying again"
          writerDone <- readIORef writerDoneRef
          if writerDone || traceShowId (Set.size seen) == iterations
             then return seen
             else readerLoop writerDoneRef inChan outChan seen
        Right actual -> do
          readerLoop writerDoneRef inChan outChan (Set.insert actual seen)
              -- | actual == expected -> do
              --     putStrLn "gotem"
              --     proceed
              -- | otherwise -> do
              --     -- actually this never happens. on the safe version of readChan it seems like it blocks until it gets the *next* value, so it recovers from an async exception?
              --     wasPreempted <- readIORef preempted
              --     if wasPreempted
              --        then do
              --          putStrLn "Preempted. Skipping. IRL we'd put the value on the end of the queue"
              --          proceed
              --        else do
              --          return (Mismatch expected actual)




countMismatches :: Int -> IntSet -> Int
countMismatches expected seen = expected - Set.size seen
