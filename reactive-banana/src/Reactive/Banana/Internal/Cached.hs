{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Internal.Cached (
    -- | Utility for executing monadic actions once
    -- and then retrieving values from a cache.
    -- 
    -- Very useful for observable sharing.
    HasST(..),
    Cached, runCached, mkCached, fromPure,
    liftCached1, liftCached2,
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe
import System.IO.Unsafe
import Data.STRef

{-----------------------------------------------------------------------------
    Cache type
------------------------------------------------------------------------------}
data Cached m a = Cached (m a)

runCached :: Cached m a -> m a
runCached (Cached x) = x

-- | Type class for monads that have ST at their base.
class (Monad m) => HasST m s | m -> s where
    liftST :: ST s a -> m a

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE mkCached #-}
mkCached :: (HasST m s, MonadFix m) => m a -> Cached m a
mkCached m = unsafePerformIO $ unsafeSTToIO $ do -- because for some reason there is no unsafePerformST
    var <- newSTRef Nothing
    return $ Cached $ do
        ma <- liftST (readSTRef var)      -- look up calculation result
        case ma of
            Nothing -> mdo
                liftST (writeSTRef var a) -- black-hole result first
                a <- m                    -- evaluate
                return a
            Just a  -> return a           -- return cached result

-- | Return a pure value.
-- Doesn't make use of the cache.
fromPure :: (HasST m s, MonadFix m) => a -> Cached m a
fromPure = Cached . return

liftCached1
    :: HasVault m
    => (a -> m b)
    -> Cached m a -> Cached m b
liftCached1 f ca = mkCached $ do
    a <- runCached ca
    f a

liftCached2
    :: HasVault m
    => (a -> b -> m c)
    -> Cached m a -> Cached m b -> Cached m c
liftCached2 f ca cb = mkCached $ do
    a <- runCached ca
    b <- runCached cb
    f a b

