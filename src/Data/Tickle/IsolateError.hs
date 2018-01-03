{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tickle.IsolateError(
  -- * Data type
  IsolateError(..)
  -- * Reduction
, isolateError
  -- * Prisms
, _NegativeSize
, _IsolateXFail
, _UnexpectedConsumed
, _Not_NegativeSize
, _Not_IsolateXFail
, _Not_UnexpectedConsumed
) where

import Papa

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
-- >>> IsolateXFail (+10) <.> IsolateXFail 99
-- IsolateXFail 109
--
-- >>> IsolateXFail (+10) <.> NegativeSize
-- NegativeSize
--
-- >>> IsolateXFail (+10) <.> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <.> UnexpectedConsumed 5 6
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <.> NegativeSize
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <.> IsolateXFail (+10)
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize <.> UnexpectedConsumed 5 6
-- NegativeSize
--
-- >>> NegativeSize <.> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize <.> IsolateXFail (+10)
-- NegativeSize
instance Apply IsolateError where
  IsolateXFail f <.> IsolateXFail x =
    IsolateXFail (f x)
  IsolateXFail _ <.> NegativeSize =
    NegativeSize
  IsolateXFail _ <.> UnexpectedConsumed x y =
    UnexpectedConsumed x y
  NegativeSize <.> _ =
    NegativeSize
  UnexpectedConsumed x y <.> _ =
    UnexpectedConsumed x y

-- |
--
-- >>> IsolateXFail (+10) <*> IsolateXFail 99
-- IsolateXFail 109
--
-- >>> IsolateXFail (+10) <*> NegativeSize
-- NegativeSize
--
-- >>> IsolateXFail (+10) <*> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <*> UnexpectedConsumed 5 6
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <*> NegativeSize
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 <*> IsolateXFail (+10)
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize <*> UnexpectedConsumed 5 6
-- NegativeSize
--
-- >>> NegativeSize <*> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize <*> IsolateXFail (+10)
-- NegativeSize
--
-- >>> pure 5 :: IsolateError Int
-- IsolateXFail 5
instance Applicative IsolateError where
  pure =
    IsolateXFail
  (<*>) =
    (<.>)

-- |
--
-- >>> IsolateXFail 99 >>- \n -> IsolateXFail (n + 10)
-- IsolateXFail 109
--
-- >>> IsolateXFail 99 >>- \_ -> NegativeSize
-- NegativeSize
--
-- >>> IsolateXFail 99 >>- \_ -> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>- \n -> IsolateXFail (n + 10)
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>- \_ -> NegativeSize
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>- \_ -> UnexpectedConsumed 5 6
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize >>- \n -> IsolateXFail (n + 10)
-- NegativeSize
--
-- >>> NegativeSize >>- \_ -> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize >>- \_ -> UnexpectedConsumed 3 4
-- NegativeSize
instance Bind IsolateError where
  IsolateXFail x >>- f =
    f x
  NegativeSize >>- _ =
    NegativeSize
  UnexpectedConsumed x y >>- _ =
    UnexpectedConsumed x y

-- |
--
-- >>> IsolateXFail 99 >>= \n -> IsolateXFail (n + 10)
-- IsolateXFail 109
--
-- >>> IsolateXFail 99 >>= \_ -> NegativeSize
-- NegativeSize
--
-- >>> IsolateXFail 99 >>= \_ -> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>= \n -> IsolateXFail (n + 10)
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>= \_ -> NegativeSize
-- UnexpectedConsumed 3 4
--
-- >>> UnexpectedConsumed 3 4 >>= \_ -> UnexpectedConsumed 5 6
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize >>= \n -> IsolateXFail (n + 10)
-- NegativeSize
--
-- >>> NegativeSize >>= \_ -> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize >>= \_ -> UnexpectedConsumed 3 4
-- NegativeSize
--
-- >>> return 5 :: IsolateError Int
-- IsolateXFail 5
instance Monad IsolateError where
  (>>=) =
    (>>-)
  return =
    pure

-- |
--
-- >>> IsolateXFail 99 <!> IsolateXFail 55
-- IsolateXFail 99
--
-- >>> IsolateXFail 99 <!> NegativeSize
-- IsolateXFail 99
--
-- >>> IsolateXFail 99 <!> UnexpectedConsumed 3 4
-- IsolateXFail 99
--
-- >>> UnexpectedConsumed 3 4 <!> IsolateXFail 55
-- IsolateXFail 55
--
-- >>> UnexpectedConsumed 3 4 <!> NegativeSize
-- NegativeSize
--
-- >>> UnexpectedConsumed 3 4 <!> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize <!> IsolateXFail 55
-- IsolateXFail 55
--
-- >>> NegativeSize <!> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize <!> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
instance Alt IsolateError where
  IsolateXFail x <!> _ =
    IsolateXFail x
  _ <!> y = 
    y

-- |
--
-- >>> IsolateXFail 99 <> IsolateXFail 55
-- IsolateXFail 99
--
-- >>> IsolateXFail 99 <> NegativeSize
-- IsolateXFail 99
--
-- >>> IsolateXFail 99 <> UnexpectedConsumed 3 4
-- IsolateXFail 99
--
-- >>> UnexpectedConsumed 3 4 <> IsolateXFail 55
-- IsolateXFail 55
--
-- >>> UnexpectedConsumed 3 4 <> NegativeSize
-- NegativeSize
--
-- >>> UnexpectedConsumed 3 4 <> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
--
-- >>> NegativeSize <> IsolateXFail 55
-- IsolateXFail 55
--
-- >>> NegativeSize <> NegativeSize
-- NegativeSize
--
-- >>> NegativeSize <> UnexpectedConsumed 3 4
-- UnexpectedConsumed 3 4
instance Semigroup (IsolateError e) where
  (<>) =
    (<!>)

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
  _Not_IsolateXFail . _Nothing

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
  Prism (IsolateError e) (IsolateError g) e g
_IsolateXFail =
  _Not_NegativeSize . _Right

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
  _Not_IsolateXFail . _Just

-- |
--
-- >>> _Not_NegativeSize # Left (8, 9)
-- UnexpectedConsumed 8 9
--
-- >>> _Not_NegativeSize # Right 'a'
-- IsolateXFail 'a'
--
-- >>> (_NegativeSize # ()) ^? _Not_NegativeSize
-- Nothing
--
-- >>> (_IsolateXFail # 8) ^? _Not_NegativeSize
-- Just (Right 8)
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _Not_NegativeSize
-- Just (Left (12,13))
_Not_NegativeSize ::
  Prism (IsolateError e) (IsolateError g) (Either (Int, Int) e) (Either (Int, Int) g)
_Not_NegativeSize =
  prism
    (\a -> case a of
             Right e -> IsolateXFail e
             Left (l, m) -> UnexpectedConsumed l m)
    (\x -> case x of
             IsolateXFail e -> Right (Right e)
             UnexpectedConsumed d e -> Right (Left (d, e))
             NegativeSize -> Left NegativeSize)

-- |
--
-- >>> _Not_IsolateXFail # Nothing
-- NegativeSize
--
-- >>> _Not_IsolateXFail # Just (7, 8)
-- UnexpectedConsumed 7 8
--
-- >>> (_NegativeSize # ()) ^? _Not_IsolateXFail
-- Just Nothing
--
-- >>> (_IsolateXFail # 8) ^? _Not_IsolateXFail
-- Nothing
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _Not_IsolateXFail
-- Just (Just (12,13))
_Not_IsolateXFail ::
  Prism' (IsolateError e) (Maybe (Int, Int))
_Not_IsolateXFail =
  prism'
    (\a -> case a of
             Nothing -> NegativeSize
             Just (l, m) -> UnexpectedConsumed l m)
    (\x -> case x of
             IsolateXFail _ -> Nothing
             UnexpectedConsumed d e -> Just (Just (d, e))
             NegativeSize -> Just Nothing)

-- |
--
-- >>> _Not_UnexpectedConsumed # Nothing
-- NegativeSize
--
-- >>> _Not_UnexpectedConsumed # Just 99
-- IsolateXFail 99
--
-- >>> (_NegativeSize # ()) ^? _Not_UnexpectedConsumed
-- Just Nothing
--
-- >>> (_IsolateXFail # 8) ^? _Not_UnexpectedConsumed
-- Just (Just 8)
--
-- >>> (_UnexpectedConsumed # (12, 13)) ^? _Not_UnexpectedConsumed
-- Nothing
_Not_UnexpectedConsumed ::
  Prism (IsolateError e) (IsolateError g) (Maybe e) (Maybe g)
_Not_UnexpectedConsumed =
  prism
    (\a -> case a of
             Nothing -> NegativeSize
             Just e -> IsolateXFail e)
    (\x -> case x of
             IsolateXFail e -> Right (Just e)
             UnexpectedConsumed d e -> Left (UnexpectedConsumed d e)
             NegativeSize -> Right Nothing)
