module TestThreadWaitQueue2 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.Datastructures.ThreadWaitQueue


type Gate = MVar ()
type Result = Int
type Results = MVar [Result]

type Threads = [Thread]

main
    = do
        startGate <- newEmptyMVar
        queue <- createWaitQueue
        resultsM <- newMVar []
        let
            amountOfOpenThreads = 20
            amountOfWaitThreads = 10
            amountOfOpens = 2000
            amountOfWaits = 200
            openThreadActions = replicate amountOfOpenThreads (openThread amountOfOpens queue startGate)
            waitThreadActions = replicate amountOfWaitThreads (waitThread amountOfWaits startGate resultsM queue)
        threads <- mapM forkThread (openThreadActions ++ waitThreadActions)
        putStrLn "Spawned everyone, lets open the gates!"
        putMVar startGate () -- Open the floods
        joinThreads threads
        putStrLn "Joined on everyone, lets look at the results"
        results <- readMVar resultsM
        putStrLn (show $ length results)
        
        
openThread :: Int -> ThreadWaitQueue -> Gate -> IO ()
openThread repeatX queue gate
    = do
        readMVar gate
        let
            actions = replicate repeatX ( do
                                            --threadDelay 10000
                                            putStrLn "put"
                                            openAndRecloseWaitQueue queue
                                        )
        sequence_ actions
        
       
       
waitThread :: Int -> Gate -> Results ->  ThreadWaitQueue -> IO ()
waitThread repeatX gate results queue
    = do
        readMVar gate
        let
            actions = replicate repeatX ( do
                                            putStrLn "take"
                                            return 1
                                        )
        testResults <- sequence actions
        resultsOld <- takeMVar results
        putMVar results (resultsOld ++ testResults)