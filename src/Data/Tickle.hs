module Data.Tickle where

import Data.Either
import Foreign
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Monad

data Buffer =
    Buffer
      {-# UNPACK #-} !(ForeignPtr Word8)
      {-# UNPACK #-} !Int                -- offset
      {-# UNPACK #-} !Int                -- used bytes
      {-# UNPACK #-} !Int                -- length left

newtype Builder =
  Builder {
      -- Invariant (from Data.ByteString.Lazy):
      --      The lists include no null ByteStrings.
    runBuilder ::
      (Buffer -> [S.ByteString])
      -> Buffer
      -> [S.ByteString]
  }      

data S =
  S
    {-# UNPACK #-} !B.ByteString  -- current chunk
    L.ByteString                  -- the rest of the input
    {-# UNPACK #-} !Int64         -- bytes read

newtype GetT e f a =
  GetT
    (S -> f (Either e (a, S)))

instance Monad f => Functor (GetT e f) where
  fmap f (GetT g) =
    GetT (liftM (liftM (liftM (\(a, s) -> (f a, s)))) g)

instance Monad f => Applicative (GetT e f) where
  pure =
    return
  (<*>) =
    ap

instance Monad f => Monad (GetT e f) where
  return a =
    GetT (\s -> return (return (a, s)))
  GetT k >>= f =
    GetT (\s -> 
      k s >>= \x -> case x of 
                      Left e ->
                        return (Left e)
                      Right (b, t) ->
                        let GetT r = f b
                        in r t)

data State a =
  State 
    {-# UNPACK #-} !Int64 -- written
    !Builder -- builder
    a -- contents

newtype PutM a =
  PutM (Int64 -> State a)
