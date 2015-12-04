{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tickle.IsolateError(
  -- * Data type
  IsolateError
  -- * Reduction
, isolateError
  -- * Prisms
, _NegativeSize
, _IsolateXFail
, _UnexpectedConsumed
) where

import Control.Lens.Prism(prism')
import Control.Lens.Type(Prism')
import Data.Eq(Eq)
import Data.Function(const)
import Data.Functor(Functor(fmap))
import Data.Functor.Extend(Extend(extended, duplicated))
import Data.Int(Int)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tuple(uncurry)
import Prelude(Show)

-- $setup
-- >>> import Control.Lens((#))
-- >>> import Control.Lens.Fold((^?))
-- >>> import Data.List(reverse)
-- >>> import Prelude(Num((+), (*)))

data IsolateError e =
  NegativeSize
  | IsolateXFail e
  | UnexpectedConsumed Int Int
  deriving (Eq, Ord, Show)

-- |
--
-- >>> isolateError 7 (\_ -> 8) (\_ _ -> 9) (_NegativeSize # ())
-- 7
--
-- >>> isolateError 7 (+10) (\_ _ -> 9) (_IsolateXFail # 99)
-- 109
--
-- >>> isolateError 7 (\_ -> 8) (+) (_UnexpectedConsumed # (12, 14))
-- 26
isolateError ::
  a
  -> (e -> a)
  -> (Int -> Int -> a)
  -> IsolateError e
  -> a
isolateError n _ _ NegativeSize =
  n
isolateError _ f _ (IsolateXFail e) =
  f e
isolateError _ _ u (UnexpectedConsumed m n) =
  u m n

-- | Map a function on the possible failure.
--
-- >>> fmap (+1) (_NegativeSize # () :: IsolateError Int)
-- NegativeSize
--
-- >>> fmap reverse (_IsolateXFail # "abc")
-- IsolateXFail "cba"
--
-- >>> fmap (+1) (_UnexpectedConsumed # (7, 8) :: IsolateError Int)
-- UnexpectedConsumed 7 8
instance Functor IsolateError where
  fmap _ NegativeSize =
    NegativeSize
  fmap f (IsolateXFail e) =
    IsolateXFail (f e)
  fmap _ (UnexpectedConsumed m n) =
    UnexpectedConsumed m n

-- | Duplicate on the possible failure.
--
-- >>> extended (isolateError 12 (+10) (*)) (_IsolateXFail # 99)
-- IsolateXFail 109
--
-- >>> extended (isolateError 12 (+10) (*)) (_UnexpectedConsumed # (12, 13))
-- UnexpectedConsumed 12 13
--
-- >>> extended (isolateError 12 (+10) (*)) (_NegativeSize # ())
-- NegativeSize
instance Extend IsolateError where
  extended _ NegativeSize =
    NegativeSize
  extended f i@(IsolateXFail _) =
    IsolateXFail (f i)
  extended _ (UnexpectedConsumed m n) =
    UnexpectedConsumed m n
  duplicated NegativeSize =
    NegativeSize
  duplicated i@(IsolateXFail _) =
    IsolateXFail i
  duplicated (UnexpectedConsumed m n) =
    UnexpectedConsumed m n

-- |
--
-- >>> _NegativeSize # ()
-- NegativeSize
--
-- >>> (_NegativeSize # ()) ^? _NegativeSize
-- Just ()
--
-- >>> (_IsolateXFail # 8) ^? _NegativeSize
-- Nothing
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _NegativeSize
-- Nothing
_NegativeSize ::
  Prism' (IsolateError e) ()
_NegativeSize =
  prism'
    (const NegativeSize)
    (\x -> case x of
             NegativeSize -> Just ()
             _ -> Nothing)

-- |
--
-- >>> _IsolateXFail # 8
-- IsolateXFail 8
--
-- >>> (_NegativeSize # ()) ^? _IsolateXFail
-- Nothing
--
-- >>> (_IsolateXFail # 8) ^? _IsolateXFail
-- Just 8
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _IsolateXFail
-- Nothing
_IsolateXFail ::
  Prism' (IsolateError e) e
_IsolateXFail =
  prism'
    IsolateXFail
    (\x -> case x of
             IsolateXFail e -> Just e
             _ -> Nothing)

-- |
--
-- >>> _UnexpectedConsumed # (12,13)
-- UnexpectedConsumed 12 13
--
-- >>> (_NegativeSize # ()) ^? _UnexpectedConsumed
-- Nothing
--
-- >>> (_IsolateXFail # 8) ^? _UnexpectedConsumed
-- Nothing
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _UnexpectedConsumed
-- Just (12,13)
_UnexpectedConsumed ::
  Prism' (IsolateError e) (Int, Int)
_UnexpectedConsumed =
  prism'
    (uncurry UnexpectedConsumed)
    (\x -> case x of
             UnexpectedConsumed l m -> Just (l, m)
             _ -> Nothing)
