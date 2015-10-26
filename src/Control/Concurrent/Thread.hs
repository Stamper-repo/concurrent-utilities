module Control.Concurrent.Thread
    ( Thread
    , forkThread
    , isTerminated
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
        tid <- forkFinally action (_setTerminated waitOn)
        return (Thread tid waitOn)


_setTerminated :: WaitOn -> (Either SomeException a) -> IO ()
_setTerminated waitOn (Right _)
    = putMVar waitOn ()
    
_setTerminated waitOn (Left someException)
    = do
        putMVar waitOn ()
        throw someException
    
isTerminated :: Thread
             -> IO Bool
isTerminated (Thread tid waitOn)
    = do
        isTerm <- isEmptyMVar waitOn
        return (not isTerm)

        
joinThread :: Thread -> IO ()
joinThread t@(Thread threadId waitOn)
    = do
        isTerm <- isTerminated t
        case isTerm of
            True -> return ()
            False -> do
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