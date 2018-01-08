module Lib where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi.Bounded
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid

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

testRun :: IO ()
testRun = do
  (inChan, outChan) <- newChan 100000
  fillChan inChan
  xsRef <- newIORef []
  forM_ [1..10000] $ \i -> do
    let n = floor $ 10000 / i
    result <- race (readChan outChan) (threadDelay n)
    case result of
      (Left val) -> modifyIORef' xsRef (val : )
      (Right _) -> putStrLn "delayed!"
  list <- readIORef xsRef
  print $ length list
  final <- checkContents (reverse list)
  print final