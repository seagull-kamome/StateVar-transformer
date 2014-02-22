{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.StateVar
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  HATTORI, HIROKI <seagull.kamome@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Data.StateVarがIO専用なのが気にくわないので変換できるようにしただけ
-- 
--------------------------------------------------------------------------------

module Data.StateVar.Trans (
   -- * Readable State Variables
   HasGetter(..),
   GettableStateVar, makeGettableStateVar,
   -- * Writable State Variables
   HasSetter(..),
   SettableStateVar, makeSettableStateVar,
   -- * General State Variables
   StateVar, makeStateVar, makePtrVar,
   -- * Utility Functions
   ($~), ($=!), ($~!),
   (&),
   (^=), (^~), (^=!), (^~!), (^.),
   (@=)
) where

import Data.IORef (IORef, readIORef, writeIORef)
import GHC.Conc (STM, TVar, readTVar, writeTVar)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Control.Monad.ST.Safe (ST)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader(..))

--------------------------------------------------------------------------------

infixr 2 $=

--------------------------------------------------------------------------------

-- | The class of all readable state variables.
class HasGetter g m | g -> m where
   -- | Read the value of a state variable.
   get :: g a -> m a

instance HasGetter IORef IO where
  get = readIORef
  {-# INLINE get #-}
instance HasGetter TVar STM where
  get = readTVar
  {-# INLINE get #-}
instance HasGetter (STRef s) (ST s) where
  get = readSTRef
  {-# INLINE get #-}

-- | A concrete implementation of a read-only state variable, carrying an IO
-- action to read the value.
newtype GettableStateVar m a = GettableStateVar (m a)

instance HasGetter (GettableStateVar m) m where
   get (GettableStateVar g) = g
   {-# INLINE get #-}

-- | Construct a 'GettableStateVar' from an IO action.
makeGettableStateVar :: m a -> GettableStateVar m a
makeGettableStateVar = GettableStateVar
{-# INLINE makeGettableStateVar #-}

--------------------------------------------------------------------------------

-- | The class of all writable state variables.
class HasSetter s m where
   -- | Write a new value into a state variable.
   ($=) :: s a -> a -> m ()

instance HasSetter IORef IO where
  ($=) = writeIORef
  {-# INLINE ($=) #-}
instance HasSetter TVar STM where
  ($=) = writeTVar
  {-# INLINE ($=) #-}
instance HasSetter (STRef s) (ST s) where
  ($=) = writeSTRef
  {-# INLINE ($=) #-}

-- | A concrete implementation of a write-only state variable, carrying an IO
-- action to write the new value.
newtype SettableStateVar m a = SettableStateVar (a -> m ())

instance HasSetter (SettableStateVar m) m where
  ($=) (SettableStateVar s) a = s a
  {-# INLINE ($=) #-}

-- | Construct a 'SettableStateVar' from an IO action.
makeSettableStateVar :: (a -> m ()) -> SettableStateVar m a
makeSettableStateVar = SettableStateVar
{-# INLINE makeSettableStateVar #-}

--------------------------------------------------------------------------------

-- | A concrete implementation of a readable and writable state variable,
-- carrying one IO action to read the value and another IO action to write the
-- new value.
data StateVar m a =
  StateVar (GettableStateVar m a) (SettableStateVar m a)

instance HasGetter (StateVar m) m where
  get (StateVar g _) = get g
  {-# INLINE get #-}
instance HasSetter (StateVar m) m where
  ($=) (StateVar _ s) a = s $= a
  {-# INLINE ($=) #-}

-- | Construct a 'StateVar' from two IO actions, one for reading and one for
-- writing.
makeStateVar :: m a -> (a -> m ()) -> StateVar m a
makeStateVar g s = StateVar (makeGettableStateVar g) (makeSettableStateVar s)
{-# INLINE makeStateVar #-}

makePtrVar :: (MonadIO m, Storable a) => Ptr a -> StateVar m a
makePtrVar p = makeStateVar (liftIO $ peek p) (liftIO . poke p)
{-# INLINE makePtrVar #-}

--------------------------------------------------------------------------------

-- | A modificator convenience function, transforming the contents of a state
-- variable with a given funtion.

($~) :: (Monad m, HasGetter v m, HasSetter v m) => v a -> (a -> a) -> m ()
v $~ f = get v >>= ($=) v . f
{-# INLINE ($~) #-}

-- | A variant of '$=' which is strict in the value to be set.
($=!) :: (Monad m, HasSetter s m) => s a -> a -> m ()
v $=! x = x `seq` v $= x
{-# INLINE ($=!) #-}

-- | A variant of '$~' which is strict in the transformed value.
($~!) :: (Monad m, HasGetter v m, HasSetter v m) => v a -> (a -> a) -> m ()
v $~! f = get v >>= ($=!) v . f
{-# INLINE ($~!) #-}

--------------------------------------------------------------------------------

(&) :: s -> (s -> t) -> t
s & t = t s
{-# INLINE (&) #-}

infixl 8 ^=, ^~, ^=!, ^~!, ^.

(^=) :: HasSetter g m => (s -> g a) -> a -> s -> m ()
(fv ^= v) s = fv s $= v
{-# INLINE (^=) #-}

(^~) :: (Monad m, HasGetter g m, HasSetter g m) => (s -> g a) -> (a -> a) -> s -> m ()
(fv ^~ f) s = v $~ f where v = fv s
{-# INLINE (^~) #-}

(^=!) :: (Monad m, HasSetter g m) => (s -> g a) -> a -> s -> m ()
(fv ^=! x) s = v $=! x where v = fv s
{-# INLINE (^=!) #-}

(^~!) :: (Monad m, HasGetter g m, HasSetter g m) => (s -> g a) -> (a ->a) -> s -> m ()
(fv ^~! f) s = v $~! f where v = fv s
{-# INLINE (^~!) #-}

(^.) :: (Monad m, HasGetter g m, HasGetter h m) => (s -> g a) -> (a -> h b) -> s -> GettableStateVar m b
(fg ^. fh) s = makeGettableStateVar $ get (fg s) >>= get . fh
{-# INLINE (^.) #-}

--------------------------------------------------------------------------------

(@=) :: (Monad m, MonadTrans n, MonadReader s (n m), HasSetter g m) => (s -> g a) -> a -> n m ()
fv @= v = ask >>= lift . (fv ^= v)
{-# INLINE (@=) #-}



