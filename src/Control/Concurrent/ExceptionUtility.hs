module Control.Concurrent.ExceptionUtility where

import Control.Exception

{- | Utility function that uses an uninterruptable mask to make sure a resource
is always acquired and released even with asynchronous exceptions. You can define
the acquire, release and update actions. The acquire function should deliver the resource.
The update function receives the acquired value and tries to update it but may run into (asynchronous) exceptions.
And lastly the release function receives the updated value (or the old value in case of an exception) and can release
the resource with it.

This function is primarily used in situations with concurrent data where some value is always required to reside
in the concurrent datastructure (ex. MVars).

The acquire and release reside under the uninterruptable mask, the update function does not and may be interrupted
or may cause exceptions.

-}
uninterruptibleUpdateResource
        :: IO a         -- ^ Acquire resource
        -> (a -> IO b)  -- ^ Release resource with either updated or old value
        -> (a -> IO a)  -- ^ How to update the value
        -> IO a         -- ^ Updated value
uninterruptibleUpdateResource acquire release update
    = uninterruptibleMask $ \restore -> do
                                            aOld <- acquire
                                            catch ( do
                                                        aNew <- restore (update aOld) 
                                                        _ <- release aNew
                                                        return aNew
                                                  )
                                                  ( \exception -> do
                                                        _ <- release aOld
                                                        throwIO (exception :: SomeException)
                                                  )
{- | A version of uninterruptibleUpdateResource where you can also calculate a new value
    
-}
uninterruptibleUpdateResourceAndCalculate
        :: IO a              -- ^ Acquire resource
        -> (a -> IO b)       -- ^ Release resource with either updated or old value
        -> (a -> IO (a, c))  -- ^ How to update the value and calculate the result
        -> IO (a, c)         -- ^ Updated value
uninterruptibleUpdateResourceAndCalculate acquire release update
    = uninterruptibleMask $ \restore -> do
                                            aOld <- acquire
                                            catch ( do
                                                        (aNew, c) <- restore (update aOld) 
                                                        _ <- release aNew
                                                        return (aNew, c)
                                                  )
                                                  ( \exception -> do
                                                        _ <- release aOld
                                                        throwIO (exception :: SomeException)
                                                  )