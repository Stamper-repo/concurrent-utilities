module TestSafePrint where

import Control.Concurrent
import Control.Concurrent.Thread
import Control.Concurrent.SafePrint

main
    = do
        safePrintToken <- createSafePrintToken
        t1 <- forkThread (putThread safePrintToken)
        t2 <- forkThread (putThread safePrintToken)
        t3 <- forkThread (putThread safePrintToken)
        safePrintLn safePrintToken "Done forking"
        joinThread t1
        joinThread t2
        joinThread t3

putThread :: SafePrintToken -> IO ()
putThread safePrintToken
    = do
        tid <- myThreadId
        threadDelay 500000
        safePrintLn safePrintToken ("Tid: " ++ (show tid))
