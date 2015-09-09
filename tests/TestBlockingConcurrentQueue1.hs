
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.Datastructures.BlockingConcurrentQueue

type TestBlockingConcurrentQueue = BlockingConcurrentQueue Int
type Lock = MVar ()

main
    = do
        putLock <- createLock
        blockingQueue <- createBlockingConcurrentQueue
        t1 <- forkThread (waitTakeThread putLock blockingQueue)
        t2 <- forkThread (waitTakeThread putLock blockingQueue)
        putThread 40 putLock blockingQueue
        putThread 59 putLock blockingQueue
        joinThread t1
        joinThread t2
        
        
putThread :: Int -> Lock -> TestBlockingConcurrentQueue -> IO ()
putThread el putLock blockingQueue
    = do
        tid <- myThreadId
        --safePut putLock ("Before put, result: " ++ (show el) ++ ", tid: " ++ (show tid))
        putInBlockingConcurrentQueue blockingQueue el
        --safePut putLock ("After put, tid : " ++ (show tid))
       
       
waitTakeThread :: Lock ->  TestBlockingConcurrentQueue -> IO ()
waitTakeThread putLock blockingQueue
    = do
        tid <- myThreadId
        --safePut putLock ("Before wait take, tid: " ++ (show tid))
        el <- takeFromBlockingConcurrentQueue blockingQueue
        safePut putLock ("After wait take, result: " ++ (show el) ++ ", tid: " ++ (show tid))
        
        
createLock :: IO Lock
createLock
    = newMVar ()
  
  
safePut :: Lock -> [Char] -> IO ()
safePut lock str
    = do
        _ <- takeMVar lock
        putStrLn str
        putMVar lock ()