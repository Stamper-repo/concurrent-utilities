
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Thread
import Control.Concurrent.Datastructures.ThreadWaitQueue

type Lock = MVar ()

main :: IO ()
main
    = do
        putLock <- createLock
        threadWaitQueue <- createWaitQueue
        t1 <- forkThread (waitThread putLock threadWaitQueue)
        t2 <- forkThread (waitThread putLock threadWaitQueue)
        openQueueThread putLock threadWaitQueue
        recloseQueueThread putLock threadWaitQueue
        t3 <- forkThread (waitThread putLock threadWaitQueue)
        t4 <- forkThread (waitThread putLock threadWaitQueue)
        openQueueThread putLock threadWaitQueue
        joinThreads [t1, t2, t3, t4]
        
        
openQueueThread :: Lock -> ThreadWaitQueue -> IO ()
openQueueThread putLock threadWaitQueue
    = do
        tid <- myThreadId
        safePut putLock ("Before open " ++ (show tid))
        threadDelay 1000000
        openWaitQueue threadWaitQueue
        safePut putLock ("After open " ++ (show tid))
        
        
recloseQueueThread :: Lock -> ThreadWaitQueue -> IO ()
recloseQueueThread putLock threadWaitQueue
    = do
        tid <- myThreadId
        safePut putLock ("Before reclose " ++ (show tid))
        recloseWaitQueue threadWaitQueue
        safePut putLock ("After reclose " ++ (show tid))
        
        
waitThread :: Lock -> ThreadWaitQueue -> IO ()
waitThread putLock threadWaitQueue
    = do
        tid <- myThreadId
        safePut putLock ("Before Wait " ++ (show tid))
        enterWaitQueue threadWaitQueue
        safePut putLock ("After wait " ++ (show tid))
        
  
createLock :: IO Lock
createLock
    = newMVar ()
  
  
safePut :: Lock -> [Char] -> IO ()
safePut lock str
    = do
        _ <- takeMVar lock
        putStrLn str
        putMVar lock ()