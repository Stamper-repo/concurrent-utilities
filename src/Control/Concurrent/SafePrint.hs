module Control.Concurrent.SafePrint where

import Control.Concurrent.MVar

type SafePrintToken = MVar ()

createSafePrintToken :: IO SafePrintToken
createSafePrintToken = newMVar ()

safePrint :: SafePrintToken -> String -> IO ()
safePrint safePrintToken message
    = do
        takeMVar safePrintToken
        putStr message
        putMVar safePrintToken ()
        
        
safePrintLn :: SafePrintToken -> String -> IO ()
safePrintLn safePrintToken message
    = do
        takeMVar safePrintToken
        putStrLn message
        putMVar safePrintToken ()
       