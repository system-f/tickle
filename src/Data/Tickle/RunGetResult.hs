{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tickle.RunGetResult(
  -- * Data type
  RunGetResult(..)
  -- * Reduction
, runGetResult
  -- * Prisms
, _RunGetFail
, _RunGet
  -- * Isomorphisms
, runGetResultEitherIso
, runGetResultValidationIso
) where

import Data.Validation(Validation, _Either)
import Papa

-- $setup
-- >>> import Data.Validation(_Success, _Failure)

data RunGetResult e a =
  RunGetFail Int64 e
  | RunGet a
  deriving (Eq, Ord, Show)

-- |
--
-- >>> runGetResult (+) (*2) (_RunGet # 12)
-- 24
--
-- >>> runGetResult (+) (*2) (_RunGetFail # (23, 12))
-- 35
runGetResult ::
  (Int64 -> e -> x)
  -> (a -> x)
  -> RunGetResult e a
  -> x
runGetResult f _ (RunGetFail i e) =
  f i e
runGetResult _ r (RunGet a) =
  r a

-- |
--
-- >>> bimap (+10) (*2) (_RunGet # 12)
-- RunGet 24
--
-- >>> bimap (+10) (*2) (_RunGetFail # (2, 3))
-- RunGetFail 2 13
instance Bifunctor RunGetResult where
  bimap f _ (RunGetFail i e) =
    RunGetFail i (f e)
  bimap _ g (RunGet a) =
    RunGet (g a)

-- |
--
-- >>> fmap (*2) (_RunGet # 8)
-- RunGet 16
--
-- >>> fmap (*2) (_RunGetFail # (9, 13))
-- RunGetFail 9 13
instance Functor (RunGetResult e) where
  fmap =
    bimap id

-- |
--
-- >>>  bifoldMap (++"abc") (++"def") (_RunGet # "hi")
-- "hidef"
--
-- >>>  bifoldMap (++"abc") (++"def") (_RunGetFail # (12, "hi"))
-- "hiabc"
instance Bifoldable RunGetResult where
  bifoldMap f _ (RunGetFail _ e) =
    f e
  bifoldMap _ g (RunGet a) =
    g a

-- |
--
-- >>> foldMap (++"abc") (_RunGet # "hi")
-- "hiabc"
--
-- >>> foldMap (++"abc") (_RunGetFail # (12, "hi"))
-- ""
instance Foldable (RunGetResult e) where
  foldMap _ (RunGetFail _ _) =
    mempty
  foldMap f (RunGet a) =
    f a

-- |
--
-- >>> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGet # 8)
-- [RunGet 56]
--
-- >>> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGet # 9)
-- [RunGet 63]
--
-- >>> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGet # 10)
-- [RunGet 11]
--
-- λ> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGetFail # (8, 18))
-- [RunGetFail 8 17]
--
-- >>> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGetFail # (8, 19))
-- [RunGetFail 8 38]
--
-- >>> bitraverse (\n -> if even n then [n - 1] else [n * 2]) (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGetFail # (8, 20))
-- [RunGetFail 8 19]
instance Bitraversable RunGetResult where
  bitraverse f _ (RunGetFail i e) =
    RunGetFail i <$> f e
  bitraverse _ g (RunGet a) =
    RunGet <$> g a

-- |
--
-- >>> traverse (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGet # 8)
-- [RunGet 56]
--
-- >>> traverse (\n -> if n `mod` 5 == 0 then [n + 1] else [n * 7]) (_RunGet # 10)
-- [RunGet 11]
instance Traversable (RunGetResult e) where
  traverse =
    bitraverse pure

-- |
--
-- >>> _RunGet # (*2) <.> _RunGet # 9
-- RunGet 18
--
-- >>> _RunGetFail # (10, 2) <.> _RunGet # 9
-- RunGetFail 10 2
--
-- >>> _RunGet # (*2) <.> _RunGetFail # (11, 3)
-- RunGetFail 11 3
--
-- >>> _RunGetFail # (10, 2) <.> _RunGetFail # (11, 3)
-- RunGetFail 10 2
instance Apply (RunGetResult e) where
  RunGet f <.> RunGet a =
    RunGet (f a)
  RunGet _ <.> RunGetFail i e =
    RunGetFail i e
  RunGetFail i e <.> _ =
    RunGetFail i e
    
-- |
--
-- >>> pure 7 :: RunGetResult () Int
-- RunGet 7
--
-- >>> _RunGet # (*2) <*> _RunGet # 9
-- RunGet 18
--
-- >>> _RunGetFail # (10, 2) <*> _RunGet # 9
-- RunGetFail 10 2
--
-- >>> _RunGet # (*2) <*> _RunGetFail # (11, 3)
-- RunGetFail 11 3
--
-- >>> _RunGetFail # (10, 2) <*> _RunGetFail # (11, 3)
-- RunGetFail 10 2
instance Applicative (RunGetResult e) where
  RunGet f <*> RunGet a =
    RunGet (f a)
  RunGet _ <*> RunGetFail i e =
    RunGetFail i e
  RunGetFail i e <*> _ =
    RunGetFail i e
  pure =
    RunGet

-- |
--
-- >>> return 7 :: RunGetResult () Int
-- RunGet 7
--
-- >>> _RunGet # 8 >>- \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGet 16
--
-- >>> _RunGet # 9 >>- \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGetFail 13 126
--
-- >>> _RunGetFail # (10, 8) >>- \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGetFail 10 8
instance Bind (RunGetResult e) where
  RunGet a >>- f =
    f a
  RunGetFail i e >>- _ =
    RunGetFail i e

-- |
--
-- >>> _RunGet # 8 >>= \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGet 16
--
-- >>> _RunGet # 9 >>= \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGetFail 13 126
--
-- >>> _RunGetFail # (10, 8) >>= \g -> if even g then _RunGet # (g * 2) else _RunGetFail # (13, g * 14)
-- RunGetFail 10 8
instance Monad (RunGetResult e) where
  RunGet a >>= f =
    f a
  RunGetFail i e >>= _ =
    RunGetFail i e
  return =
    RunGet

-- |
--
-- >>> _RunGet # 8 <!> _RunGet # 9
-- RunGet 8
--
-- >>> _RunGet # 8 <!> _RunGetFail # (10, 9)
-- RunGet 8
--
-- >>> _RunGetFail # (10, 8) <!> _RunGet # 9
-- RunGet 9
--
-- >>> _RunGetFail # (10, 8) <!> _RunGet # (20, 9)
-- RunGet (20,9)
instance Alt (RunGetResult e) where
  RunGet a <!> _ =
    RunGet a
  RunGetFail _ _ <!> r =
    r

-- |
--
-- >>> duplicated (_RunGet # 8)
-- RunGet (RunGet 8)
--
-- >>> duplicated (_RunGetFail # (9, "abc"))
-- RunGetFail 9 "abc"
instance Extend (RunGetResult e) where
  extended _ (RunGetFail i e) =
    RunGetFail i e
  extended f g@(RunGet _) =
    RunGet (f g)
  duplicated (RunGetFail i e) = 
    RunGetFail i e
  duplicated g@(RunGet _) =
    RunGet g

-- |
--
-- >>> _RunGet # 8 <> _RunGet # 9
-- RunGet 8
--
-- >>> _RunGet # 8 <> _RunGetFail # (10, 9)
-- RunGet 8
--
-- >>> _RunGetFail # (10, 8) <> _RunGet # 9
-- RunGet 9
--
-- >>> _RunGetFail # (10, 8) <> _RunGet # (20, 9)
-- RunGet (20,9)
instance Semigroup (RunGetResult e a) where
  (<>) =
    (<!>)

-- |
--
-- >>> _RunGetFail # (10, "abc")
-- RunGetFail 10 "abc"
--
-- >>> (_RunGet # 8) ^? _RunGetFail
-- Nothing
--
-- >>> (_RunGetFail # (8, "abc")) ^? _RunGetFail
-- Just (8,"abc")
_RunGetFail ::
  Prism' (RunGetResult e a) (Int64, e)
_RunGetFail =
  prism'
    (uncurry RunGetFail)
    (\x -> case x of
             RunGetFail i e -> Just (i, e)
             _ -> Nothing)

-- |
--
-- >>> (_RunGet # 8) ^? _RunGet
-- Just 8
--
-- >>> (_RunGetFail # (8, "abc")) ^? _RunGet
-- Nothing
_RunGet ::
  Prism' (RunGetResult e a) a
_RunGet =
  prism'
    RunGet
    (\x -> case x of
             RunGet a -> Just a
             _ -> Nothing)

-- |
--
-- >>> runGetResultEitherIso # Right 99
-- RunGet 99
--
-- >>> runGetResultEitherIso # Left (12, "abc")
-- RunGetFail 12 "abc"
--
-- >>> from runGetResultEitherIso # _RunGet # 99
-- Right 99
--
-- >>> from runGetResultEitherIso # _RunGetFail # (12, "abc")
-- Left (12,"abc")
runGetResultEitherIso ::
  Iso (RunGetResult a b) (RunGetResult b d) (Either (Int64, a) b) (Either (Int64, b) d)
runGetResultEitherIso =
  iso
    (\r -> case r of
             RunGetFail i a -> Left (i, a)
             RunGet b -> Right b)
    (\e -> case e of
             Left (i, a) -> RunGetFail i a
             Right b -> RunGet b)

-- |
--
-- >>> runGetResultValidationIso # _Success # 99
-- RunGet 99
--
-- >>> runGetResultValidationIso # _Failure # (12, "abc")
-- RunGetFail 12 "abc"
--
-- >>> from runGetResultValidationIso # _RunGet # 99
-- Success 99
--
-- >>> from runGetResultValidationIso # _RunGetFail # (12, "abc")
-- Failure (12,"abc")
runGetResultValidationIso ::
  Iso (RunGetResult a b) (RunGetResult b d) (Validation (Int64, a) b) (Validation (Int64, b) d)
runGetResultValidationIso =
  from (_Either . from runGetResultEitherIso)
