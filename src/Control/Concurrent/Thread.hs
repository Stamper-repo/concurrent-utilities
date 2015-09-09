module Control.Concurrent.Thread
    ( Thread
    , forkThread
    , joinThread
    , joinThreads
    , getThreadId
    , terminateThread
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

type WaitOn = MVar ()
data Thread = Thread ThreadId WaitOn

instance Show Thread where
    show (Thread threadId _) = show threadId


forkThread :: IO () -> IO Thread
forkThread action 
    = do
        waitOn <- newEmptyMVar
        tid <- forkFinally action (setTerminated waitOn)
        return (Thread tid waitOn)

        
setTerminated :: WaitOn -> (Either SomeException a) -> IO ()
setTerminated waitOn (Right _)
    = putMVar waitOn ()
    
setTerminated waitOn (Left someException)
    = do
        putMVar waitOn ()
        throw someException
    
        
        
joinThread :: Thread -> IO ()
joinThread (Thread threadId waitOn)
    = do
        _ <- takeMVar waitOn
        return ()

        
joinThreads :: [Thread] -> IO ()
joinThreads threads = foldr ((>>).joinThread) (return ()) threads
        
        
getThreadId :: Thread -> ThreadId
getThreadId (Thread tid _)
    = tid
        
        
terminateThread :: Thread -> IO ()
terminateThread (Thread tid _)
    = do
        killThread tid