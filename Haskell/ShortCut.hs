{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module ShortCut where

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Class
import Control.Monad.Trans

{-
Helps to implement imperative algorithms in a monadic context,
introducing a shortcut computation. The shortcut behaves
like a the `return` statement in imperative languages, it
terminates the computation with the given final result.
-}

-- ShortCut Monad Transformer
newtype ShortCutT r m a = ShortCutT { unShortCutT :: m (Either r a) }

-- ShortCut Monad
type ShortCut r a = ShortCutT r Identity a

-- Run a shortcut computation if does not computes
-- the result in a shortcut way, it returns the
-- given default value.
runShortCutT m d = do
  result <- unShortCutT m
  case result of
    Left result   -> return result
    Right partial -> return d

-- Runs a shortcut computation which result could come
-- from the final shortcut computation, or the result
-- of the normal computation.
runShortCutT' :: (Monad m) => ShortCutT r m r -> m r
runShortCutT' m = do
  result <- unShortCutT m
  case result of
    Left result    -> return result
    Right computed -> return computed

-- Change context in the return computation
changeContext :: (Monad m) => (r -> s) -> ShortCutT r m a -> ShortCutT s m a
changeContext f (ShortCutT m) = ShortCutT $ do
  result <- m
  return $ case result of
    Left result -> Left (f result)
    Right x     -> Right x

instance (Functor m, Monad m) => Functor (ShortCutT r m) where
  fmap f (ShortCutT m) = ShortCutT $ fmap (fmap f) m

instance (Applicative m, Monad m, Monad (ShortCutT r m)) => Applicative (ShortCutT r m) where
  pure  = ShortCutT . pure . Right
  (<*>) = ap

instance Monad m => Monad (ShortCutT r m) where
    return  = ShortCutT . return . Right
    m >>= k = ShortCutT $ do
      r <- unShortCutT m
      case r of
        Left result -> return $ Left result
        Right a -> unShortCutT (k a)

-- Sets the final value of the computation
-- and terminates the computation
final :: (Monad m) => r -> ShortCutT r m a
final = ShortCutT . return . Left

-- Same as final
result :: (Monad m) => r -> ShortCutT r m a
result = final

-- Catche the final value of the computation
-- and converts it to a partial `Left result`, otherwise
-- computes `Right a`
partial' :: (Monad m) => ShortCutT r m a -> ShortCutT r m (Either r a)
partial' m = ShortCutT $ do
  r <- unShortCutT m
  return . Right $ case r of
    Left  r -> Left r
    Right a -> Right a

-- Convert an `Left r` to a final result, or `Right a`
-- to a partial result of computation
either :: (Monad m) => (Either r a) -> ShortCutT r m a
either (Left r)  = final r
either (Right a) = return a

-- Catches the final value of the computation
-- and converts it to a partial result, or
-- use the result of the normal computation
partial :: (Monad m) => ShortCutT r m r -> ShortCutT r m r
partial m = ShortCutT $ do
  r <- unShortCutT m
  return . Right $ case r of
    Left  r -> r
    Right r -> r

instance MonadTrans (ShortCutT r) where
  lift = ShortCutT . mmap Right

-- {-# LANGUAGE UndecidableInstances  #-}
instance MonadState s m => MonadState s (ShortCutT r m) where
  get = lift get
  put = lift . put
  state = lift . state

-- * Helpers

-- Same as fmap, just using the monadic context
mmap :: Monad m => (a -> b) -> m a -> m b
mmap f m = m >>= return . f
