
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

type TestBlockingConcurrentQueue = BlockingConcurrentQueue Result

type Gate = MVar ()
type Result = Int
type Results = MVar [Result]

type Threads = [Thread]

main
    = do
        startGate <- newEmptyMVar
        blockingQueue <- createBlockingConcurrentQueue
        resultsM <- newMVar []
        let
            amountOfPutThreads = [1..1]
            amountOfTakeThreads = [1..2]
            amountOfPuts = 600
            amountOfTakes = 200
            putThreadActions = map (putThread amountOfPuts blockingQueue startGate) amountOfPutThreads
            takeThreadActions = map (\_ -> takeThread amountOfTakes startGate resultsM blockingQueue) amountOfTakeThreads
            forkableActions = map forkThread (putThreadActions ++ takeThreadActions)
        threads <- mapM forkThread (putThreadActions ++ takeThreadActions)
        putStrLn "Spawned everyone, lets open the gates!"
        putMVar startGate () -- Open the floods
        joinThreads threads
        putStrLn "Joined on everyone, lets look at the results"
        results <- readMVar resultsM
        putStrLn (show $ length results)
        
        
putThread :: Int -> TestBlockingConcurrentQueue -> Gate -> Result -> IO ()
putThread repeatX blockingQueue gate result 
    = do
        readMVar gate
        putStrLn "Starting putting"
        let
            actions = replicate repeatX ( do
                                            threadDelay 10000
                                            --putStrLn "put"
                                            putInBlockingConcurrentQueue blockingQueue result
                                        )
        sequence_ actions
        putStrLn "Done putting"
       
       
takeThread :: Int -> Gate -> Results ->  TestBlockingConcurrentQueue -> IO ()
takeThread repeatX gate results blockingQueue
    = do
        readMVar gate
        putStrLn "Starting taking"
        let
            actions = replicate repeatX ( do
                                            --putStrLn "take"
                                            --threadDelay 100
                                            takeAllFromBlockingConcurrentQueue blockingQueue
                                        )
        testResults <- sequence actions
        resultsOld <- takeMVar results
        putMVar results (resultsOld ++ (concat testResults))
        putStrLn "Done taking"
        
