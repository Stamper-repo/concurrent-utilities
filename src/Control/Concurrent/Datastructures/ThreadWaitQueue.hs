module Control.Concurrent.Datastructures.ThreadWaitQueue where

import Control.Concurrent
import Control.Concurrent.MVar

type WaitOnToken = MVar ()
type ThreadWaitQueue = MVar WaitOnToken


createWaitQueue :: IO ThreadWaitQueue
createWaitQueue
    = do
        waitOnToken <- newEmptyMVar
        threadWaitQueue <- newMVar waitOnToken
        return threadWaitQueue

        
getQueueTicket :: ThreadWaitQueue -> IO WaitOnToken
getQueueTicket threadWaitQueue
    = readMVar threadWaitQueue
    
    
enterWaitQueueWithTicket :: WaitOnToken -> IO ()
enterWaitQueueWithTicket waitOnToken
    = do
        yield 
                {- Do not remove this yield. This yield is the product of many many tears 
                 and a newfound disgust of the concurrent forkIO Haskell system.
                 Now more serious:
                 forkIO creates lightweight threads that are NOT interrupted when they are
                 performing a computation. The switch happens somewhere else (probably after a 
                 task has been completed). So a very deep recursion tree is not interrupted until it
                 is solved.
                 So picture the following case: the queue is open and a lightweight thread tried
                 to perform something that the queue was necessary for and finds that the condition isn't
                 set yet and returns to the queue. But the queue is open... so we start all over again
                 and again and again... and because the computation is not "complete" the thread is never
                 switched out and thus we have a blocking thread that causes the entire system to halt.
                 This yield interrupts that process and gives other threads actually the chance to set the
                 condition the queue is needed for.
              -}
        readMVar waitOnToken
        
        
enterWaitQueue :: ThreadWaitQueue -> IO ()
enterWaitQueue threadWaitQueue
    = getQueueTicket threadWaitQueue >>= enterWaitQueueWithTicket

    
{- | Open the queue. If the queue is already opened, nothing happens.
-}
openWaitQueue :: ThreadWaitQueue -> IO ()
openWaitQueue threadWaitQueue
    = do
        waitOnToken <- takeMVar threadWaitQueue
        _openIfNeeded waitOnToken
        putMVar threadWaitQueue waitOnToken
        
        
{- Not thread safe! Should never be called if we do not have the rights
for the ThreadWaitQueue
-}
_openIfNeeded :: WaitOnToken -> IO ()
_openIfNeeded waitOnToken
    = do
        notOpenYet <- isEmptyMVar waitOnToken
        case notOpenYet of
            True -> putMVar waitOnToken ()
            False -> return ()
            
            
recloseWaitQueue :: ThreadWaitQueue -> IO ()
recloseWaitQueue threadWaitQueue
    = do
        waitOnToken' <- newEmptyMVar
        _ <- takeMVar threadWaitQueue
        putMVar threadWaitQueue waitOnToken'
        
        
{- | Atomic
-}
openAndRecloseWaitQueue :: ThreadWaitQueue -> IO ()
openAndRecloseWaitQueue threadWaitQueue
    = do
        waitOnToken1 <- takeMVar threadWaitQueue
        waitOnToken2 <- newEmptyMVar
        -- First open the old queue if needed
        _openIfNeeded waitOnToken1
        -- Now set new close queue
        putMVar threadWaitQueue waitOnToken2

