{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.StateVar
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- State variables are references in the IO monad, like 'IORef's or parts of
-- the OpenGL state. Note that state variables are not neccessarily writable or
-- readable, they may come in read-only or write-only flavours, too. As a very
-- simple example for a state variable, consider an explicitly allocated memory
-- buffer. This buffer can easily be converted into a 'StateVar':
--
-- @
-- makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
-- makeStateVarFromPtr p = makeStateVar (peek p) (poke p)
-- @
--
-- The example below puts 11 into a state variable (i.e. into the buffer),
-- increments the contents of the state variable by 22, and finally prints the
-- resulting content:
--
-- @
--   do p <- malloc :: IO (Ptr Int)
--      let v = makeStateVarFromPtr p
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--
-- 'IORef's are state variables, too, so an example with them looks extremely
-- similiar:
--
-- @
--   do v <- newIORef (0 :: Int)
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--------------------------------------------------------------------------------

module Data.StateVar.Trans (
   -- * Readable State Variables
   HasGetter(..),
   GettableStateVar, makeGettableStateVar,
   -- * Writable State Variables
   HasSetter(..),
   SettableStateVar, makeSettableStateVar,
   -- * General State Variables
   StateVar, makeStateVar,
   -- * Utility Functions
   ($~), ($=!), ($~!),
   (&), (^=), (^~), (^=!), (^~!)
) where

import Data.IORef ( IORef, readIORef, writeIORef )

--------------------------------------------------------------------------------

infixr 2 $=

--------------------------------------------------------------------------------

-- | The class of all readable state variables.
class HasGetter g m | g -> m where
   -- | Read the value of a state variable.
   get :: g a -> m a

instance HasGetter IORef IO where
   get = readIORef

-- | A concrete implementation of a read-only state variable, carrying an IO
-- action to read the value.
newtype GettableStateVar m a = GettableStateVar (m a)

instance HasGetter (GettableStateVar m) m where
   get (GettableStateVar g) = g

-- | Construct a 'GettableStateVar' from an IO action.
makeGettableStateVar :: m a -> GettableStateVar m a
makeGettableStateVar = GettableStateVar

--------------------------------------------------------------------------------

-- | The class of all writable state variables.
class HasSetter s m where
   -- | Write a new value into a state variable.
   ($=) :: s a -> a -> m ()

instance HasSetter IORef IO where
   ($=) = writeIORef

-- | A concrete implementation of a write-only state variable, carrying an IO
-- action to write the new value.
newtype SettableStateVar m a = SettableStateVar (a -> m ())

instance HasSetter (SettableStateVar m) m where
   ($=) (SettableStateVar s) a = s a

-- | Construct a 'SettableStateVar' from an IO action.
makeSettableStateVar :: (a -> m ()) -> SettableStateVar m a
makeSettableStateVar = SettableStateVar

--------------------------------------------------------------------------------

-- | A concrete implementation of a readable and writable state variable,
-- carrying one IO action to read the value and another IO action to write the
-- new value.
data StateVar m a =
   StateVar (GettableStateVar m a) (SettableStateVar m a)

instance HasGetter (StateVar m) m where
   get (StateVar g _) = get g

instance HasSetter (StateVar m) m where
   ($=) (StateVar _ s) a = s $= a

-- | Construct a 'StateVar' from two IO actions, one for reading and one for
-- writing.
makeStateVar :: m a -> (a -> m ()) -> StateVar m a
makeStateVar g s = StateVar (makeGettableStateVar g) (makeSettableStateVar s)

--------------------------------------------------------------------------------

-- | A modificator convenience function, transforming the contents of a state
-- variable with a given funtion.

($~) :: (Monad m, HasGetter v m, HasSetter v m) => v a -> (a -> a) -> m ()
v $~ f = do
  x <- get v
  v $= f x

-- | A variant of '$=' which is strict in the value to be set.
($=!) :: (Monad m, HasSetter s m) => s a -> a -> m ()
v $=! x = x `seq` v $= x

-- | A variant of '$~' which is strict in the transformed value.
($~!) :: (Monad m, HasGetter v m, HasSetter v m) => v a -> (a -> a) -> m ()
v $~! f = do
   x <- get v
   v $=! f x

--------------------------------------------------------------------------------

(&) :: s -> (s -> t) -> t
s & t = t s

(^=) :: HasSetter v m => (s -> v a) -> a -> s -> m ()
(fv ^= v) s = fv s $= v

(^~) :: (Monad m, HasGetter v m, HasSetter v m) => (s -> v a) -> (a -> a) -> s -> m ()
(fv ^~ f) s = v $~ f where v = fv s

(^=!) :: (Monad m, HasSetter v m) => (s -> v a) -> a -> s -> m ()
(fv ^=! x) s = v $=! x where v = fv s

(^~!) :: (Monad m, HasGetter v m, HasSetter v m) => (s -> v a) -> (a ->a) -> s -> m ()
(fv ^~! f) s = v $~! f where v = fv s

