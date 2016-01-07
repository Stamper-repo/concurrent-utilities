module Control.Concurrent.ExceptionCollection
    ( ExceptionCollection(..)
    , createExceptionCollection
    , logException
    , readExceptions
    , collectExceptions
    , hasExceptions
    ) where

import Control.Concurrent.MVar

data ExceptionCollection e = ExceptionCollection (MVar [e])


createExceptionCollection :: IO (ExceptionCollection e)
createExceptionCollection
    = do
        exceptionsM <- newMVar []
        return (ExceptionCollection exceptionsM)

        
logException :: ExceptionCollection e
             -> e
             -> IO ()
logException (ExceptionCollection exceptionsM) e
    = do
        raisedExceptions <- takeMVar exceptionsM
        putMVar exceptionsM (raisedExceptions ++ [e])


hasExceptions :: ExceptionCollection e
              -> IO Bool
hasExceptions (ExceptionCollection exceptionsM)
    = do
        raisedExceptions <- readMVar exceptionsM
        return (not $ null raisedExceptions)


readExceptions :: ExceptionCollection e
               -> IO [e]
readExceptions (ExceptionCollection exceptionsM)
    = readMVar exceptionsM
    

collectExceptions :: ExceptionCollection e
                  -> IO [e]
collectExceptions (ExceptionCollection exceptionsM)
    = do
        raisedExceptions <- takeMVar exceptionsM
        putMVar exceptionsM []
        return raisedExceptions