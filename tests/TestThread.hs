module TestThread where

import Control.Concurrent
import Control.Concurrent.Thread
import Control.Exception

main :: IO ()
main = do
        thread1 <- forkThread (threadDelay 1000000)
        putStrLn ("Before normal")
        joinThread thread1
        putStrLn ("After normal")
        thread2 <- forkThread (threadDelay 1000000)
        putStrLn ("Before Exception")
        throwTo (getThreadId thread2) Overflow
        joinThread thread2
        putStrLn ("After Exception")


