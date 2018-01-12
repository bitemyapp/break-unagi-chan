{-# LANGUAGE RankNTypes #-}
module Main where

-- import Lib
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded as UB
import           Control.Monad
import           Data.IORef
import           Data.Monoid
import           Debug.Trace
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random.MWC
import           System.Timeout

main :: IO ()
main = do
  args <- getArgs
  case args of
    [giveUp, chanSize, iterations] -> do
      res <- fuzzTest (read giveUp) (read chanSize) (read iterations) safeAlgo
      case res of
        OK -> putStrLn "Success!"
        Mismatch expected actual -> do
          bail ("MISMATCH, expected " <> show expected <> " but got " <> show actual)
    _ -> usage
  where
    usage = bail "ARGS: giveUp (microseconds) chanSize iterations, e.g. 500000 10000 100"
    bail msg = do
      hPutStrLn stderr msg
      exitFailure


-- | Blockingly reads the chan
simpleAlgo :: IO () -> InChan a -> OutChan a -> IO a
simpleAlgo _ _ = UB.readChan


--FIXME: this may be a non-starter, we cannot put what we got back on
--the front of the queue. TBMQueue supports unGet but we don't need
--that strong of a guarantee in the implementation because its fine if
--the logs get sent slightly out of order.

-- | When interrupted, tries to un-take the value
safeAlgo :: IO () -> InChan a  -> OutChan a -> IO a
safeAlgo preempted inChan outChan = UB.readChanOnException outChan tryGiveBack
  where
    tryGiveBack getDropped = do
      putStrLn "readChan got interrupted. Data would be lost with readChan"
      -- uhh, somehow with a relatively high giveUp, 500ms, this just keeps trucking. when I'd expect it would put this on the end of the queue and get a mismatch. For lower delays like 50ms it falls on its face
      -- void . tryWriteChan inChan =<< getDropped
      -- uh does this only work if i call getDropped?
      _ <- getDropped
      preempted


fuzzTest
  :: Int
  -- ^ Give up timeout in loop
  -> Int
  -- ^ Chan size, set it quite high
  -> Int
  -- ^ Iterations
  -> (forall a. IO () -> InChan a -> OutChan a -> IO a)
  -> IO Result
fuzzTest giveUp chanSize iterations algo = do
  gen <- createSystemRandom
  (inChan, outChan) <- UB.newChan chanSize
  writer <- async (writeLoop gen inChan 0)
  reader <- async (readerLoop inChan outChan 0)
  link2 writer reader
  res <- wait reader
  cancel writer
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
    readerLoop inChan outChan expected
      | expected > iterations = return OK
      | otherwise = do
          preempted <- newIORef False
          let signalPreempted = writeIORef preempted True
          res <- race (threadDelay giveUp) (algo signalPreempted inChan outChan)
          let proceed = readerLoop inChan outChan (expected + 1)
          case res of
            Left _ -> do
              wasPreempted <- readIORef preempted
              if wasPreempted
                 then do
                   putStrLn "Preempted. Skipping. IRL we'd put the value on the end of the queue"
                   proceed
                 else do
                   putStrLn "Gave up. Trying again"
                   readerLoop inChan outChan expected
            Right actual
              | actual == expected -> do
                  putStrLn "gotem"
                  proceed
              | otherwise -> do
                  -- actually this never happens. on the safe version of readChan it seems like it blocks until it gets the *next* value, so it recovers from an async exception?
                  wasPreempted <- readIORef preempted
                  if wasPreempted
                     then do
                       putStrLn "Preempted. Skipping. IRL we'd put the value on the end of the queue"
                       proceed
                     else do
                       return (Mismatch expected actual)


data Result = Mismatch Int Int | OK
