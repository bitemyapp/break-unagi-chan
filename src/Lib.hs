module Lib where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import System.Timeout

fillChan :: InChan Int -> IO ()
fillChan inChan = writeList2Chan inChan [1..10000]

checkContents :: [Int] -> IO Int
checkContents xs =
  foldlM (\old new -> do
             case succ old == new of
               False -> do
                 putStrLn $ "SKIP DETECTED: " <> show old <> " " <> show new
                 return new
               True -> return new
            )
  0 xs

-- calc :: IO Integer
-- calc =
--   evaluate $ 10000000000 + 10000000000

-- testRun :: IO ()
-- testRun = undefined

testRun :: IO ()
testRun = do
  (inChan, outChan) <- newChan 100000
  fillChan inChan
  xsRef <- newIORef []
  -- forM_ [1..10000] $ \i -> do
  --   let n = floor $ 10000 / i
  --   result <- race (readChan outChan) (threadDelay n)
  --   case result of
  --     (Left val) ->
  --       modifyIORef' xsRef (val : )
  --     (Right _) ->
  --       putStrLn $ "delayed on " <> show i

  -- exception handling variant --
  let handler getter = do
        val <- getter
        writeChan inChan val
  getMaskingState >>= print
  forM_ [1..10000] $ \i -> do
    let n = floor $ 10000 / i
    result <- race
              -- (readChan outChan)
              -- (mask_ $ readChan outChan)
              -- (uninterruptibleMask $ \_ -> readChan outChan)
              (do
                  -- when (i == 5000) $ getMaskingState >>= print
                  (uninterruptibleMask_ $ do
                      when (i == 5000) $ getMaskingState >>= print
                      readChan outChan))
              -- ((getMaskingState >>= print) >> (uninterruptibleMask_ $ print <$> getMaskingState >> readChan outChan))
              (threadDelay n)
    case result of
      (Left val) ->
        modifyIORef' xsRef (val : )
      (Right _) ->
        putStrLn $ "delayed on " <> show i
    -- result <- timeout n (readChan outChan)
    -- result <- timeout n (readChanOnException outChan handler)
    -- (_, blockForValue) <- tryReadChan outChan
    -- result <- timeout n blockForValue
    -- case result of
    --   (Just val) ->
    --     modifyIORef' xsRef (val : )
    --   Nothing ->
    --     putStrLn $ "delayed on " <> show i

  list <- readIORef xsRef
  print $ length list
  final <- checkContents (reverse list)
  print final
  -- finalContents <- getChanContents outChan
  -- print finalContents
  chanLength <- estimatedLength inChan
  print chanLength
  (elem, _) <- tryReadChan outChan
  maybeVal <- tryRead elem
  print maybeVal
