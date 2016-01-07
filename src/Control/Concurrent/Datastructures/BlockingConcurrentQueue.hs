module Control.Concurrent.Datastructures.BlockingConcurrentQueue where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Datastructures.ThreadWaitQueue


data BlockingConcurrentQueue a = BlockingConcurrentQueue { queue :: MVar [a]
                                                         , threadWaitQueue :: ThreadWaitQueue -- Used to signal when to try again if the queue was empty and we wanted to take one
                                                         }

instance Show (BlockingConcurrentQueue a) where
    show _ = "BlockingConcurrentQueue"
                                 

showBlockingConcurrentQueue :: (Show a) => BlockingConcurrentQueue a -> IO [Char]
showBlockingConcurrentQueue blockingConcurrentQueue
    = do
        els <- readAllFromBlockingConcurrentQueue blockingConcurrentQueue
        return (show els)
            

createBlockingConcurrentQueue :: IO (BlockingConcurrentQueue a)
createBlockingConcurrentQueue
    = do
        newQueue <- newMVar []
        threadWaitQueue <- createWaitQueue
        return (BlockingConcurrentQueue newQueue threadWaitQueue)

                     
putInBlockingConcurrentQueue :: BlockingConcurrentQueue a -> a -> IO ()
putInBlockingConcurrentQueue blockingConcurrentQueue el
    = do
        oldQueue <- takeMVar (queue blockingConcurrentQueue)
        putMVar (queue blockingConcurrentQueue) (oldQueue ++ [el])
        openAndRecloseWaitQueue (threadWaitQueue blockingConcurrentQueue)

        
putAllInBlockingConcurrentQueue :: BlockingConcurrentQueue a -> [a] -> IO ()
putAllInBlockingConcurrentQueue blockingConcurrentQueue els
    = do
        oldQueue <- takeMVar (queue blockingConcurrentQueue)
        putMVar (queue blockingConcurrentQueue) (oldQueue ++ els)
        openAndRecloseWaitQueue (threadWaitQueue blockingConcurrentQueue)

        
takeFromBlockingConcurrentQueue :: BlockingConcurrentQueue a -> IO a
takeFromBlockingConcurrentQueue blockingConcurrentQueue
    = do
        oldQueue <- takeMVar (queue blockingConcurrentQueue)
        case oldQueue of
            []       -> do
                            queueTicket <- getQueueTicket (threadWaitQueue blockingConcurrentQueue)
                            putMVar (queue blockingConcurrentQueue) []
                            enterWaitQueueWithTicket queueTicket
                            takeFromBlockingConcurrentQueue blockingConcurrentQueue
            (el:els) -> do
                            putMVar (queue blockingConcurrentQueue) els
                            return el

                            
takeAllFromBlockingConcurrentQueue :: BlockingConcurrentQueue a -> IO [a]
takeAllFromBlockingConcurrentQueue blockingConcurrentQueue
    = do
        oldQueue <- takeMVar (queue blockingConcurrentQueue)
        case oldQueue of
            []       -> do
                            queueTicket <- getQueueTicket (threadWaitQueue blockingConcurrentQueue)
                            putMVar (queue blockingConcurrentQueue) []
                            enterWaitQueueWithTicket queueTicket
                            takeAllFromBlockingConcurrentQueue blockingConcurrentQueue
            (el:els) -> do
                            putMVar (queue blockingConcurrentQueue) []
                            return (el:els)
        
    
readAllFromBlockingConcurrentQueue :: BlockingConcurrentQueue a -> IO [a]
readAllFromBlockingConcurrentQueue blockingConcurrentQueue
    = readMVar (queue blockingConcurrentQueue)