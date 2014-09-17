{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Tickle.Get(
  -- * Get data type
  Get
  -- ** Primitive parsers
, getLazyByteString
, getLazyByteStringNul
, getRemainingLazyByteString
, getPtr
, getWord8
, getWord16be
, getWord16le
, getWord32be
, getWord32le
, getWord64be
, getWord64le
, getWordhost
, getWord16host
, getWord32host 
, getWord64host
, failGet
, constant
, bytesRead
, demandInput
, skip
, isNotEmpty
, isEmpty
, getByteString
, modify
, readN
, ensureN
  -- ** Higher-level combinators
, runAndKeepTrack
, pushBack
, pushFront
, xrunGetIncremental
, noMeansNo
, prompt
, isolate
, lookAhead
, lookAheadM
, lookAheadE
, readNWith
, calculateOffset
, pushChunk
, pushChunks
, pushEndOfInput
  -- ** Error label
, (!+)
, addLabel
, (!-)
, setLabel
, (!!-)
, modifyLabel
  -- * Decoder
, Decoder
, decoder
, _Fail
, _Partial
, _Done
  -- ** Run Get parser
, runGet
, (.>>)
, (<<.)
, runGetIncremental
  -- * XDecoder data type
, XDecoder
  -- ** Reduction
, xdecoder
  -- ** Prisms
, _XFail
, _XPartial
, _XDone
, _XBytesRead
  -- * CompletedXDecoder data type
, CompletedXDecoder
  -- ** Reduction
, completedXDecoder
  -- ** Prism
, _CompletedFail
, _CompletedDone
  -- ** Isomorphism
, completedIso
  -- ** Lens
, completedByteString
, completedValue
  -- ** Prism
, uncomplete
  -- ** Traversal
, uncompletedByteString
, uncompletedValue
) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Control.Category(Category((.), id))
import Control.Lens.Iso(iso)
import Control.Lens.Lens(lens)
import Control.Lens.Prism(prism')
import Control.Lens.Review((#))
import Control.Lens.Type(Iso, Prism', Lens', Traversal')
import Control.Monad(Monad((>>=), (>>), return), ap)
import Data.Bifoldable(Bifoldable(bifoldMap))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Bits((.|.))
import Data.Bool(Bool(False, True), (&&), not)
import qualified Data.ByteString as B(ByteString, concat, append, length, splitAt, empty, null, break, drop)
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L(ByteString, toChunks, fromChunks)
import qualified Data.ByteString.Lazy.Internal as LI(ByteString(Chunk, Empty))
import qualified Data.ByteString.Unsafe as BU(unsafeDrop, unsafeTake, unsafeHead, unsafeIndex, unsafeUseAsCString)
import Data.Either(Either(Left, Right), either)
import Data.Eq(Eq((==)))
import Data.Foldable(Foldable(foldMap))
import Data.Function(const)
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Alt(Alt((<!>)))
import qualified Data.Functor.Alt as Al(Alt(some, many))
import Data.Functor.Bind(Bind((>>-)))
import Data.Int(Int, Int64)
import Data.List(reverse)
import Data.Maybe(Maybe(Nothing, Just), maybe, isJust)
import Data.Monoid(Monoid(mempty))
import Data.Ord(Ord((>), (>=), (<), (>=)))
import Data.Semigroup(Semigroup((<>)))
import Data.Tickle.IsolateError(IsolateError, _NegativeSize, _IsolateXFail, _UnexpectedConsumed)
import Data.Tickle.RunGetResult(RunGetResult, _RunGet, _RunGetFail)
import Data.Traversable(Traversable(traverse))
import Data.Tuple(uncurry)
import Foreign(Ptr, castPtr, Storable(peek), sizeOf)
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Word(Word, Word8, Word16(W16#), Word32(W32#), Word64(W64#))
import GHC.Base(uncheckedShiftL#, Int(I#))
#endif
import Prelude(Num((-), (+)), ($!), Show, fromIntegral, undefined)
import System.IO(IO)

-- $setup
-- >>> import Control.Lens.Fold((^?))
-- >>> import Control.Lens.Prism(_Right, _Left)
-- >>> import qualified Data.ByteString.Lazy.Char8 as BLC(pack)
-- >>> import qualified Data.ByteString.Char8 as BC(ByteString, pack)
-- >>> import Data.String(String)
-- >>> import Data.List((++))
-- >>> import Data.Maybe(fromMaybe, isNothing)
-- >>> import Data.Validation(_Success, _Failure)
-- >>> import Prelude(Num((*)), subtract, even, mod)

newtype Get e a = 
  Get (forall r.
    B.ByteString ->
    (B.ByteString -> a -> XDecoder e r) ->
    XDecoder e r)

bimapG ::
  (e -> f)
  -> (a -> b)
  -> Get e a
  -> Get f b
bimapG f g (Get z) =
  Get (\b q -> 
    let r = z b XDone
        go (XDone i a) = q i (g a)
        go (XPartial k) = XPartial (go . k)
        go (XFail i s) = XFail i (f s) 
        go (XBytesRead u k) = XBytesRead u (go . k)
    in go r)
{-# INLINE bimapG #-}

-- | Map on the error and result of a @Get@ decoder.
--
-- >>> runGet (bimap (const True) (\x -> x + x) getWord8) (BLC.pack "")
-- RunGetFail 0 True
--
-- >>> runGet (bimap (const True) (\x -> x + x) getWord8) (BLC.pack "abc")
-- RunGet 194
instance Bifunctor Get where
  bimap =
    bimapG

fmapG ::
  (a -> b)
  -> Get e a
  -> Get e b
fmapG =
  bimapG id
{-# INLINE fmapG #-}

-- | Map on the result of a @Get@ decoder.
--
-- >>> runGet (fmap (\x -> x + x) getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (\x -> x + x) getWord8) (BLC.pack "abc")
-- RunGet 194
instance Functor (Get e) where
  fmap =
    fmapG

-- | Apply a function on the @Get@ decoder result.
--
-- >>> runGet (fmap (+) getWord8 <.> getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (+) getWord8 <.> getWord8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (fmap (+) getWord8 <.> getWord8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (fmap (+) getWord8 <.> getWord8) (BLC.pack "abc")
-- RunGet 195
instance Apply (Get e) where
  (<.>) =
    ap

apG ::
  Get e (a -> b)
  -> Get e a
  -> Get e b
apG d e =
  do b <- d
     a <- e
     return (b a)
{-# INLINE [0] apG #-}

-- | Apply a function on the @Get@ decoder result.
--
-- >>> runGet (fmap (+) getWord8 <*> getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (+) getWord8 <*> getWord8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (fmap (+) getWord8 <*> getWord8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (fmap (+) getWord8 <*> getWord8) (BLC.pack "abc")
-- RunGet 195
--
-- >>> runGet (pure 7 :: Get () Int) (BLC.pack "abc")
-- RunGet 7
--
-- prop> runGet (pure x :: Get () Int) (BLC.pack "abc") == _RunGet # x
instance Applicative (Get e) where
  pure =
    return
  {-# INLINE pure #-}
  (<*>) =
    apG
  {-# INLINE (<*>) #-}

-- | Sequence an action through the @Get@ decoder.
--
-- >>> runGet (getWord8 >>- \c1 -> fmap (\c2 -> c1 + c2) getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getWord8 >>- \c1 -> fmap (\c2 -> c1 + c2) getWord8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (getWord8 >>- \c1 -> fmap (\c2 -> c1 + c2) getWord8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (getWord8 >>- \c1 -> fmap (\c2 -> c1 + c2) getWord8) (BLC.pack "abc")
-- RunGet 195
instance Bind (Get e) where
  (>>-) =
    (>>=)

returnG ::
  a
  -> Get e a
returnG a =
    Get (\b q -> q b a)
{-# INLINE [0] returnG #-}

bindG ::
  Get e a
  -> (a -> Get e b)
  -> Get e b
Get k `bindG` f =
  Get (\b q -> k b (\c a -> 
    let Get l = f a 
    in l c q))
{-# INLINE bindG #-}

-- | Sequence an action through the @Get@ decoder.
--
-- >>> runGet (return 7 :: Get () Int) (BLC.pack "abc")
-- RunGet 7
--
-- prop> runGet (return x :: Get () Int) (BLC.pack "abc") == _RunGet # x
--
-- >>> runGet (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2)) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2)) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2)) (BLC.pack "abc")
-- RunGet 195
instance Monad (Get e) where
  return =
    returnG
  (>>=) =
    bindG

-- | Pick between two @Get@ decoders, finding the first to not fail.
--
-- >>> runGet ((+1) <$> getWord8 <!> subtract 1 <$> getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet ((+1) <$> getWord8 <!> subtract 1 <$> getWord8) (BLC.pack "abc")
-- RunGet 98
--
-- >>> runGet (getWord8 <!> failGet ()) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getWord8 <!> failGet ()) (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet (Al.some getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (Al.some getWord8) (BLC.pack "a")
-- RunGet [97]
--
-- >>> runGet (Al.some getWord8) (BLC.pack "abc")
-- RunGet [97,98,99]
--
-- >>> runGet (Al.many getWord8) (BLC.pack "")
-- RunGet []
--
-- >>> runGet (Al.many getWord8) (BLC.pack "a")
-- RunGet [97]
--
-- >>> runGet (Al.many getWord8) (BLC.pack "abc")
-- RunGet [97,98,99]
instance Alt (Get e) where
  f <!> g = 
    do (d, bs) <- runAndKeepTrack f
       case d of
         CompletedDone b a -> Get (\_ q -> q b a)
         CompletedFail _ _ -> pushBack bs >> g
  some p =
    (:) <$> p <*> Al.many p
  many p =
    (p >>= \x ->
     fmap (x:) (Al.many p)) <!> pure []
  
-- | Pick between two @Get@ decoders, finding the first to not fail.
--
-- >>> runGet (((+1) <$> getWord8) <> (subtract 1 <$> getWord8)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (((+1) <$> getWord8) <> (subtract 1 <$> getWord8)) (BLC.pack "abc")
-- RunGet 98
--
-- >>> runGet (getWord8 <> failGet ()) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getWord8 <> failGet ()) (BLC.pack "abc")
-- RunGet 97
instance Semigroup (Get e a) where
  (<>) =
    (<!>)

-- | A @Get@ decoder that always fails with the given value.
--
-- prop> runGet (failGet x :: Get Int ()) (BLC.pack s) == _RunGetFail # (0, x)
--
-- >>> runGet (failGet "abc" :: Get String ()) (BLC.pack "def")
-- RunGetFail 0 "abc"
failGet ::
  e
  -> Get e a
failGet e =
  Get (\i _ -> XFail i e)

constant ::
  (forall r. XDecoder e r)
  -> Get e a
constant d =
  Get (\_ _ -> d)

-- | Run a @Get@ decoder, but keep a track of the input that ran it to completion.
--
-- >>> runGet (runAndKeepTrack getWord8 :: Get () (CompletedXDecoder () Word8, [BC.ByteString])) (BLC.pack "")
-- RunGet (CompletedFail "" (),[])
--
-- >>> runGet (runAndKeepTrack getWord8 :: Get () (CompletedXDecoder () Word8, [BC.ByteString])) (BLC.pack "abc")
-- RunGet (CompletedDone "bc" 97,["abc"])
runAndKeepTrack ::
  Get e a
  -> Get x (CompletedXDecoder e a, [B.ByteString])
runAndKeepTrack (Get k) =
  Get (\b q -> 
    let go !acc w = case w of
                      XDone c a -> q b (CompletedDone c a, reverse acc)
                      XPartial l -> XPartial (\c -> go (maybe acc (:acc) c) (l c))
                      XFail c e -> q b (CompletedFail c e, reverse acc)
                      XBytesRead i l -> XBytesRead i (go acc . l)
    in go [] (k b XDone))
{-# INLINE runAndKeepTrack #-}

-- |
--
-- >>> runGet (pushBack [] :: Get () ()) (BLC.pack "")
-- RunGet ()
--
-- >>> runGet (pushBack [] :: Get () ()) (BLC.pack "abc")
-- RunGet ()
--
-- >>> runGet (pushBack [BC.pack "def"] :: Get () ()) (BLC.pack "")
-- RunGet ()
--
-- >>> runGet (pushBack [BC.pack "def"] :: Get () ()) (BLC.pack "abc")
-- RunGet ()
pushBack ::
  [B.ByteString]
  -> Get e ()
pushBack z =
  let y [] = id
      y _ = B.concat . (:z)
  in Get (\b q -> q (y z b) ())
{-# INLINE pushBack #-}

-- |
--
-- >>> runGet (pushFront (BC.pack "def") :: Get () ()) (BLC.pack "")
-- RunGet ()
--
-- >>> runGet (pushFront (BC.pack "def") :: Get () ()) (BLC.pack "abc")
-- RunGet ()
pushFront ::
  B.ByteString
  -> Get e ()
pushFront b =
  Get (\c q -> q (B.append b c) ())
{-# INLINE pushFront #-}

xrunGetIncremental ::
  Get e a
  -> XDecoder e a
xrunGetIncremental (Get k) =
  noMeansNo (k B.empty XDone)

noMeansNo ::
  XDecoder e a
  -> XDecoder e a
noMeansNo = 
  let neverAgain (XPartial k) =
        neverAgain (k Nothing)
      neverAgain (XBytesRead i k) =
        XBytesRead i (neverAgain . k)
      neverAgain r@(XDone _ _) =
        r
      neverAgain r@(XFail _ _) =
        r
      go (XPartial k) =
        XPartial (\b -> 
          (if isJust b then go else neverAgain) (k b))
      go (XBytesRead i k) =
        XBytesRead i (go . k)
      go r@(XDone _ _) =
        r
      go r@(XFail _ _) =
        r
  in go

prompt ::
  B.ByteString
  -> XDecoder e a
  -> (B.ByteString -> XDecoder e a)
  -> XDecoder e a
prompt b d f =
  let loop = XPartial (maybe d (\s -> if B.null s then loop else f (b `B.append` s)))
  in loop

-- |
--
-- >>> runGet (bytesRead :: Get () Int64) (BLC.pack "")
-- RunGet 0
--
-- >>> runGet (bytesRead :: Get () Int64) (BLC.pack "abc")
-- RunGet 0
--
-- >>> runGet (getWord8 >> getWord16be >> getWord32le >> bytesRead) (BLC.pack "abcdefghijk")
-- RunGet 7
bytesRead ::
  Get e Int64
bytesRead =
  Get (\b q -> XBytesRead (fromIntegral (B.length b)) (q b))

-- |
--
-- >>> runGet (isolate 1 getWord8) (BLC.pack "ab")
-- RunGet 97
--
-- >>> runGet (isolate 1 getWord8) (BLC.pack "abcde")
-- RunGet 97
--
-- >>> runGet (isolate 2 getWord16le) (BLC.pack "abcde")
-- RunGet 25185
--
-- >>> runGet (isolate 1 getWord16le) (BLC.pack "abcde")
-- RunGetFail 0 (IsolateXFail ())
--
-- >>> runGet (isolate (-3) getWord16le) (BLC.pack "abcde")
-- RunGetFail 0 NegativeSize
--
-- >>> runGet (isolate 3 getWord16le) (BLC.pack "abcde")
-- RunGetFail 2 (UnexpectedConsumed 2 3)
isolate ::
  Int
  -> Get e a
  -> Get (IsolateError e) a
isolate m (Get k) =
  let go !n (XDone l x) =
        if n == 0 && B.null l
          then
            return x
          else
            do pushFront l
               failGet (_UnexpectedConsumed # (m - n - B.length l, m))
      go 0 (XPartial r) =
        go 0 (r Nothing)
      go n (XPartial r) =
        do i <- Get (\b q -> let takeLimited t =
                                   let (j, o) = B.splitAt n t
                                   in q o (Just j)
                             in if B.null b
                                  then
                                    prompt b (q B.empty Nothing) takeLimited
                                  else
                                    takeLimited b)
           case i of
             Nothing ->
               go n (r Nothing)
             Just u ->
               go (n - B.length u) (r (Just u))       
      go _ (XFail b e) =
        pushFront b >> failGet (_IsolateXFail # e)
      go n (XBytesRead i r) =
        go n (r $! fromIntegral m - fromIntegral n - i)
  in if m < 0
       then
         failGet (_NegativeSize # ())
       else
         go m (k B.empty XDone)

-- |
--
-- >>> runGet demandInput (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet demandInput (BLC.pack "a")
-- RunGet ()
--
-- >>> runGet demandInput (BLC.pack "abc")
-- RunGet ()
demandInput ::
  Get () ()
demandInput =
  Get (\b q ->
    prompt b (XFail b ()) (`q` ()))

-- |
--
-- >>> runGet (getWord8 >>= \c -> skip 2 >> getWord8 >>= \d -> return (c,d)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getWord8 >>= \c -> skip 2 >> getWord8 >>= \d -> return (c,d)) (BLC.pack "abcdefghi")
-- RunGet (97,100)
--
-- >>> runGet (getWord8 >>= \c -> skip 2 >> getWord8 >>= \d -> return (c,d)) (BLC.pack "abc")
-- RunGetFail 3 ()
skip ::
  Int
  -> Get () ()
skip n =
  readN n (return ())
{-# INLINE skip #-}

-- |
--
-- >>> runGet isNotEmpty (BLC.pack "")
-- RunGet False
--
-- >>> runGet isNotEmpty (BLC.pack "abc")
-- RunGet True
--
-- >>> runGet (isNotEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "abc")
-- RunGet (97,True)
--
-- >>> runGet (isNotEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (isNotEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "a")
-- RunGet (97,True)
isNotEmpty ::
  Get e Bool
isNotEmpty =
  fmap not isEmpty

-- |
--
-- >>> runGet isEmpty (BLC.pack "")
-- RunGet True
--
-- >>> runGet isEmpty (BLC.pack "abc")
-- RunGet False
--
-- >>> runGet (isEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "abc")
-- RunGet (97,False)
--
-- >>> runGet (isEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (isEmpty >>= \p -> getWord8 >>= \w -> return (w, p)) (BLC.pack "a")
-- RunGet (97,False)
isEmpty ::
  Get e Bool
isEmpty =
  Get (\b q ->
    if B.null b
      then
        prompt b (q b True) (`q` False)
      else
        q b False)

-- |
--
-- >>> runGet (lookAhead getWord8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (lookAhead getWord8) (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet (lookAhead getWord8) (BLC.pack "a")
-- RunGet 97
lookAhead ::
  Get e a
  -> Get e a
lookAhead g =
  do (d, bs) <- runAndKeepTrack g
     case d of
       CompletedDone _ a ->
         pushBack bs >> return a
       CompletedFail inp s ->
         constant (XFail inp s)

-- |
--
-- >>> runGet (lookAheadM (getWord8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "abc")
-- RunGet Nothing
--
-- >>> runGet (lookAheadM (getWord8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "bc")
-- RunGet (Just 103)
--
-- >>> runGet (lookAheadM (getWord8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "")
-- RunGetFail 0 ()
lookAheadM ::
  Get e (Maybe a)
  -> Get e (Maybe a)
lookAheadM g =
  let g' = fmap (maybe (Left ()) Right) g
  in fmap (either (return Nothing) Just) (lookAheadE g')

-- |
--
-- >>> runGet (lookAheadE (getWord8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "abc")
-- RunGet (Right 93)
--
-- >>> runGet (lookAheadE (getWord8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "bc")
-- RunGet (Left 103)
--
-- >>> runGet (lookAheadE (getWord8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "")
-- RunGetFail 0 ()
lookAheadE ::
  Get e (Either a b)
  -> Get e (Either a b)
lookAheadE g =
  do (d, b) <- runAndKeepTrack g
     case d of
       CompletedDone _ (Left x) ->
         pushBack b >> return (Left x)
       CompletedDone i (Right x) ->
         Get (\_ q -> q i (Right x))
       CompletedFail i s ->
         constant (XFail i s)

-- |
--
-- >>> runGet ([(), ()] !+ (setLabel [] getWord8)) (BLC.pack "")
-- RunGetFail 0 [(),()]
--
-- >>> runGet ([(), ()] !+ (setLabel [] getWord8)) (BLC.pack "abc")
-- RunGet 97
(!+) ::
  Semigroup e =>
  e
  -> Get e a
  -> Get e a
(!+) =
  addLabel

infixl 3 !+

-- |
--
-- >>> runGet ([(), ()] `addLabel` (setLabel [] getWord8)) (BLC.pack "")
-- RunGetFail 0 [(),()]
--
-- >>> runGet ([(), ()] `addLabel` (setLabel [] getWord8)) (BLC.pack "abc")
-- RunGet 97
addLabel ::
  Semigroup e =>
  e
  -> Get e a
  -> Get e a
addLabel m =
  modifyLabel (<> m)

-- |
--
-- >>> runGet ("error" !- getWord8) (BLC.pack "")
-- RunGetFail 0 "error"
--
-- >>> runGet ("error" !- getWord8) (BLC.pack "abc")
-- RunGet 97
(!-) ::
  e
  -> Get d a
  -> Get e a
(!-) =
  setLabel

infixl 3 !-

-- |
--
-- >>> runGet ("error" `setLabel` getWord8) (BLC.pack "")
-- RunGetFail 0 "error"
--
-- >>> runGet ("error" `setLabel` getWord8) (BLC.pack "abc")
-- RunGet 97
setLabel ::
  e
  -> Get d a
  -> Get e a
setLabel =
  modifyLabel . return

-- |
--
-- >>> runGet (reverse !!- setLabel "error" getWord8) (BLC.pack "")
-- RunGetFail 0 "rorre"
--
-- >>> runGet (reverse !!- setLabel "error" getWord8) (BLC.pack "abc")
-- RunGet 97
(!!-) ::
  (d -> e)
  -> Get d a
  -> Get e a
(!!-) =
  modifyLabel

infixl 3 !!-

-- |
--
-- >>> runGet (reverse `modifyLabel` setLabel "error" getWord8) (BLC.pack "")
-- RunGetFail 0 "rorre"
--
-- >>> runGet (reverse `modifyLabel` setLabel "error" getWord8) (BLC.pack "abc")
-- RunGet 97
modifyLabel ::
  (d -> e)
  -> Get d a
  -> Get e a
modifyLabel m =
  bimap m id

-- |
--
-- >>> runGet (getByteString (-3)) (BLC.pack "")
-- RunGet ""
--
-- >>> runGet (getByteString 3) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getByteString 3) (BLC.pack "abc")
-- RunGet "abc"
--
-- >>> runGet (getByteString 3) (BLC.pack "abcdef")
-- RunGet "abc"
getByteString ::
  Int
  -> Get () B.ByteString
getByteString n =
  if n > 0
    then 
      readN n (BU.unsafeTake n)
    else
      return B.empty
{-# INLINE getByteString #-}

modify ::
  (B.ByteString -> B.ByteString)
  -> Get e ()
modify f =
  Get (\b q -> q (f b) ())

get ::
  Get e B.ByteString
get =
  Get (\b q -> q b b)

put ::
  B.ByteString -> Get e ()
put s = 
  Get (\_ q -> q s ())

-- |
--
-- >>> runGet (readN 3 id) (BLC.pack "abc")
-- RunGet "abc"
--
-- >>> runGet (readN 3 id) (BLC.pack "ab")
-- RunGetFail 0 ()
--
-- >>> runGet (readN 3 id) (BLC.pack "abcdef")
-- RunGet "abcdef"
--
-- >>> runGet (readN (-3) id) (BLC.pack "abcdef")
-- RunGet ""
readN ::
  Int
  -> (B.ByteString -> a)
  -> Get () a
readN !n f =
  ensureN n >> unsafeReadN n f
{-# INLINE [0] readN #-}

{-# RULES

"readN/readN merge" forall n m f g.
  readN n f `apG` readN m g =
    readN (n+m) (\bs -> f bs (g (BU.unsafeDrop n bs)))

"returnG/readN swap" [~1] forall f.
  returnG f =
    readN 0 (const f)

"readN 0/returnG swapback" [1] forall f.
  readN 0 f =
    returnG (f B.empty) 

  #-}

-- |
--
-- >>> runGet (ensureN 3) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (ensureN 3) (BLC.pack "abc")
-- RunGet ()
--
-- >>> runGet (ensureN 3) (BLC.pack "abcdef")
-- RunGet ()
ensureN ::
  Int
  -> Get () ()
ensureN !m =
  Get (\i k -> 
    let go n =
          Get (\b q -> if B.length b >= n
                         then
                           q b ()
                         else
                           let Get g = demandInput >> go n
                           in g b q)
    in if B.length i >= m
         then
           k i ()
         else
           let Get g = go m
           in g i k)
{-# INLINE ensureN #-}

unsafeReadN ::
  Int 
  -> (B.ByteString -> a)
  -> Get e a
unsafeReadN !n f = 
  Get (\b ks -> ks (BU.unsafeDrop n b) $! f b)

readNWith ::
  Int
  -> (Ptr a -> IO a)
  -> Get () a
readNWith n f =
  readN n (\s -> BI.inlinePerformIO (BU.unsafeUseAsCString s (f . castPtr)))
{-# INLINE readNWith #-}

data Decoder e a =
  Fail !B.ByteString {-# UNPACK #-} !Int64 e
  | Partial (Maybe B.ByteString -> Decoder e a)
  | Done !B.ByteString {-# UNPACK #-} !Int64 a

-- |
--
-- >>> decoder (\b i n -> B.length b + fromIntegral i + n) (\_ -> 99) (\b n a -> B.length b + fromIntegral n + a) (_Fail # (BC.pack "abc", 12, 19))
-- 34
--
-- >>> decoder (\b i n -> B.length b + fromIntegral i + n) (\_ -> 99) (\b n a -> B.length b + fromIntegral n + a) (_Partial # (\b -> _Fail # (fromMaybe (BC.pack "abc") b, 12, 19)))
-- 99
--
-- >>> decoder (\b i n -> B.length b + fromIntegral i + n) (\_ -> 99) (\b n a -> B.length b + fromIntegral n + a) (_Done # (BC.pack "abc", 12, 19))
-- 34
decoder :: 
  (B.ByteString -> Int64 -> e -> x)
  -> ((Maybe B.ByteString -> Decoder e a) -> x)
  -> (B.ByteString -> Int64 -> a -> x)
  -> Decoder e a
  -> x
decoder f _ _ (Fail b i e) =
  f b i e
decoder _ p _ (Partial k) =
  p k
decoder _ _ d (Done b i a) =
  d b i a

-- |
--
-- >>> (_Fail # (BC.pack "abc", 19, 31)) ^? _Fail
-- Just ("abc",19,31)
--
-- >>> isNothing ((_Fail # (BC.pack "abc", 19, 31)) ^? _Partial)
-- True
--
-- >>> (_Fail # (BC.pack "abc", 19, 31)) ^? _Done
-- Nothing
_Fail ::
  Prism' (Decoder e a) (B.ByteString, Int64, e)
_Fail =
  prism'
    (\(b, i, e) -> Fail b i e)
    (\x -> case x of
             Fail b i e -> Just (b, i, e)
             _ -> Nothing)

-- |
--
-- >>> (_Partial # (\b -> _Fail # (fromMaybe (BC.pack "abc") b, 12, 19))) ^? _Fail
-- Nothing
--
-- >>> isJust ((_Partial # (\b -> _Fail # (fromMaybe (BC.pack "abc") b, 12, 19))) ^? _Partial)
-- True
--
-- >>> (_Partial # (\b -> _Fail # (fromMaybe (BC.pack "abc") b, 12, 19))) ^? _Done
-- Nothing
_Partial ::
  Prism' (Decoder e a) (Maybe B.ByteString -> Decoder e a)
_Partial =
  prism'
    Partial
    (\x -> case x of
             Partial k -> Just k
             _ -> Nothing)

-- |
--
-- >>> (_Done # (BC.pack "abc", 19, 31)) ^? _Fail
-- Nothing
--
-- >>> isNothing ((_Done # (BC.pack "abc", 19, 31)) ^? _Partial)
-- True
--
-- >>> (_Done # (BC.pack "abc", 19, 31)) ^? _Done
-- Just ("abc",19,31)
_Done ::
  Prism' (Decoder e a) (B.ByteString, Int64, a)
_Done =
  prism'
    (\(b, i, a) -> Done b i a)
    (\x -> case x of
             Done b i a -> Just (b, i, a)
             _ -> Nothing)

bimapD ::
  (e -> f)
  -> (a -> b)
  -> Decoder e a
  -> Decoder f b
bimapD f _ (Fail b i e) =
  Fail b i (f e)
bimapD f g (Partial k) =
  Partial (bimapD f g . k)
bimapD _ g (Done b i a) =
  Done b i (g a)
{-# INLINE bimapD #-}

-- |
--
-- >>> (bimap (+10) (*20) (_Fail # (BC.pack "abc", 19, 31))) ^? _Fail
-- Just ("abc",19,41)
--
-- >>> (bimap (+10) (*20) (_Done # (BC.pack "abc", 19, 31))) ^? _Done
-- Just ("abc",19,620)
instance Bifunctor Decoder where
  bimap =
    bimapD

fmapD ::
  (a -> b)
  -> Decoder e a
  -> Decoder e b
fmapD =
  bimapD id
{-# INLINE fmapD #-}

-- |
--
-- >>> (fmap (+10) (_Fail # (BC.pack "abc", 19, 31))) ^? _Fail
-- Just ("abc",19,31)
--
-- >>> (fmap (+10) (_Done # (BC.pack "abc", 19, 31))) ^? _Done
-- Just ("abc",19,41)
instance Functor (Decoder e) where
  fmap =
    fmapD

calculateOffset :: 
  XDecoder e a
  -> Decoder e a
calculateOffset s =
  let go r !acc =
        case r of
          XDone i a ->
            Done i (acc - fromIntegral (B.length i)) a
          XFail i e ->
            Fail i (acc - fromIntegral (B.length i)) e
          XPartial k ->
            Partial (\b ->
              case b of
                Nothing -> go (k Nothing) acc
                Just j -> go (k b) (acc + fromIntegral (B.length j)))
          XBytesRead i k ->
            go (k $! (acc - i)) acc
  in go s 0

runGetIncremental ::
  Get e a
  -> Decoder e a
runGetIncremental =
  calculateOffset . xrunGetIncremental

takeHeadChunk ::
  L.ByteString
  -> Maybe B.ByteString
takeHeadChunk lbs =
  case lbs of
    (LI.Chunk bs _) ->
      Just bs
    _ ->
      Nothing

dropHeadChunk ::
  L.ByteString
  -> L.ByteString
dropHeadChunk lbs =
  case lbs of
    (LI.Chunk _ lbs') ->
      lbs'
    _ ->
      LI.Empty

-- | An alias for @runGet@.
--
-- >>> (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2)) .>> BLC.pack "abc"
-- RunGet 195
(.>>) ::
  Get e a
  -> L.ByteString
  -> RunGetResult e a
(.>>) =
  runGet 

infixl 2 .>>

-- | An alias for @runGet@ with the arguments flipped.
--
-- >>> BLC.pack "abc" <<. (getWord8 >>= \c1 -> getWord8 >>= \c2 -> return (c1 + c2))
-- RunGet 195
(<<.) ::
  L.ByteString
  -> Get e a
  -> RunGetResult e a
(<<.) b =
  (`runGet` b)

infixl 2 <<.

runGet ::
  Get e a
  -> L.ByteString
  -> RunGetResult e a
runGet g b =
  let feedAll (Done _ _ x) _ =
        _RunGet # x
      feedAll (Partial k) c =
        feedAll (k (takeHeadChunk c)) (dropHeadChunk c)
      feedAll (Fail _ p e) _ =
        _RunGetFail # (p, e)
  in feedAll (runGetIncremental g) b
    
pushChunk ::
  Decoder e a
  -> B.ByteString
  -> Decoder e a
pushChunk r i =
  case r of
    Done j p a ->
      Done (j `B.append` i) p a
    Partial k ->
      k (Just i)
    Fail j p s ->
      Fail (j `B.append` i) p s

pushChunks ::
  Decoder e a
  -> L.ByteString
  -> Decoder e a
pushChunks r0 =
  let go r [] =
        r
      go (Done i p a) xs =
        Done (B.concat (i:xs)) p a
      go (Fail i p s) xs =
        Fail (B.concat (i:xs)) p s
      go (Partial k) (x:xs) =
        go (k (Just x)) xs
  in go r0 . L.toChunks
    
pushEndOfInput ::
  Decoder e a
  -> Decoder e a
pushEndOfInput r =
  case r of
    Done {} -> r
    Partial k -> k Nothing
    Fail {} -> r

-- |
--
-- >>> runGet (getLazyByteString 5) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (getLazyByteString 5) (BLC.pack "abc")
-- RunGetFail 3 ()
--
-- >>> runGet (getLazyByteString 5) (BLC.pack "abcdefg")
-- RunGet "abcde"
getLazyByteString ::
  Int64
  -> Get () L.ByteString
getLazyByteString =
  let consume n s =
        if fromIntegral (B.length s) >= n
          then
            Right (B.splitAt (fromIntegral n) s)
          else
            Left (fromIntegral (B.length s))
      go n =
        do s <- get
           case consume n s of
             Left u ->
               do put B.empty
                  demandInput
                  fmap (s:) (go (n - u))
             Right (w, r) ->
               do put r
                  return [w]
  in fmap L.fromChunks . go

-- |
--
-- >>> runGet getLazyByteStringNul (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet getLazyByteStringNul (BLC.pack "abc")
-- RunGetFail 3 ()
--
-- >>> runGet getLazyByteStringNul (BLC.pack "abc\0")
-- RunGet "abc"
--
-- >>> runGet getLazyByteStringNul (BLC.pack "abc\0def")
-- RunGet "abc"
getLazyByteStringNul ::
  Get () L.ByteString
getLazyByteStringNul =
  let findNull s =
        case B.break (==0) s of
          (w, r) ->
            if B.null r 
              then
                Nothing
              else
                Just (w, B.drop 1 r)
      go =
        do s <- get 
           case findNull s of
             Nothing ->
               do put B.empty
                  demandInput
                  fmap (s:) go
             Just (w, r) -> 
               do put r
                  return [w]
  in fmap L.fromChunks go

-- |
--
-- >>> runGet getRemainingLazyByteString  (BLC.pack "")
-- RunGet ""
--
-- >>> runGet getRemainingLazyByteString  (BLC.pack "abc")
-- RunGet "abc"
getRemainingLazyByteString ::
  Get e L.ByteString
getRemainingLazyByteString =
  let go =
        do s <- get
           put B.empty
           d <- isEmpty
           if d
             then 
               return [s]
             else
               fmap (s:) go
  in fmap L.fromChunks go

getPtr ::
  Storable a =>
  Int
  -> Get () a
getPtr n =
  readNWith n peek
{-# INLINE getPtr #-}

{-# RULES

"getWord8/readN" getWord8 =
  readN 1 BU.unsafeHead
  
"getWord16be/readN" getWord16be =
  readN 2 word16be

"getWord16le/readN" getWord16le =
  readN 2 word16le

"getWord32be/readN" getWord32be =
  readN 4 word32be

"getWord32le/readN" getWord32le =
  readN 4 word32le

"getWord64be/readN" getWord64be =
  readN 8 word64be

"getWord64le/readN" getWord64le =
  readN 8 word64le

  #-}

-- |
--
-- >>> runGet getWord8 (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet getWord8 (BLC.pack "123")
-- RunGet 49
getWord8 ::
  Get () Word8
getWord8 =
  readN 1 BU.unsafeHead
{-# INLINE [0] getWord8 #-}

word16be ::
  B.ByteString
  -> Word16
word16be s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW16` 8) .|.
    fromIntegral (s `BU.unsafeIndex` 1)
{-# INLINE word16be #-}

-- |
--
-- >>> runGet getWord16be (BLC.pack "abc")
-- RunGet 24930
--
-- >>> runGet getWord16be (BLC.pack "123")
-- RunGet 12594
getWord16be ::
  Get () Word16
getWord16be =
  readN 2 word16be
{-# INLINE [0] getWord16be #-}

word16le ::
  B.ByteString
  -> Word16
word16le s =
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW16` 8) .|.
    fromIntegral (s `BU.unsafeIndex` 0)
{-# INLINE word16le #-}

-- |
--
-- >>> runGet getWord16le (BLC.pack "abc")
-- RunGet 25185
--
-- >>> runGet getWord16le (BLC.pack "123")
-- RunGet 12849
getWord16le ::
  Get () Word16
getWord16le =
  readN 2 word16le
{-# INLINE [0] getWord16le #-}

word32be ::
  B.ByteString
  -> Word32
word32be s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW32` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW32` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW32`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 3)
{-# INLINE word32be #-}

-- |
--
-- >>> runGet getWord32be (BLC.pack "abcdef")
-- RunGet 1633837924
--
-- >>> runGet getWord32be (BLC.pack "123456")
-- RunGet 825373492
getWord32be ::
  Get () Word32
getWord32be =
  readN 4 word32be
{-# INLINE [0] getWord32be #-}

word32le ::
  B.ByteString
  -> Word32
word32le s =
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW32` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW32` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW32`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 0)
{-# INLINE word32le #-}

-- |
--
-- -- >>> runGet getWord32le (BLC.pack "abcdef")
-- RunGet 1684234849
--
-- >>> runGet getWord32le (BLC.pack "123456")
-- RunGet 875770417
getWord32le ::
  Get () Word32
getWord32le =
  readN 4 word32le
{-# INLINE [0] getWord32le #-}

word64be ::
  B.ByteString
  -> Word64
word64be s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW64` 56) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW64` 48) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW64` 40) .|.
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW64` 32) .|.
    (fromIntegral (s `BU.unsafeIndex` 4) `shiftlW64` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 5) `shiftlW64` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 6) `shiftlW64`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 7)
{-# INLINE word64be #-}

-- |
--
-- >>> runGet getWord64be (BLC.pack "abcdefghi")
-- RunGet 7017280452245743464
--
-- >>> runGet getWord64be (BLC.pack "123456789")
-- RunGet 3544952156018063160
getWord64be ::
  Get () Word64
getWord64be =
  readN 8 word64be
{-# INLINE [0] getWord64be #-}

word64le ::
  B.ByteString
  -> Word64
word64le s =
    (fromIntegral (s `BU.unsafeIndex` 7) `shiftlW64` 56) .|.
    (fromIntegral (s `BU.unsafeIndex` 6) `shiftlW64` 48) .|.
    (fromIntegral (s `BU.unsafeIndex` 5) `shiftlW64` 40) .|.
    (fromIntegral (s `BU.unsafeIndex` 4) `shiftlW64` 32) .|.
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW64` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW64` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW64`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 0) 
{-# INLINE word64le #-}

-- |
--
-- >>> runGet getWord64le (BLC.pack "abcdefghi")
-- RunGet 7523094288207667809
--
-- >>> runGet getWord64le (BLC.pack "123456789")
-- RunGet 4050765991979987505
getWord64le ::
  Get () Word64
getWord64le =
  readN 8 word64le
{-# INLINE [0] getWord64le #-}

-- |
--
-- >>> runGet getWordhost (BLC.pack "abcdefghi")
-- RunGet 7523094288207667809
--
-- >>> runGet getWordhost (BLC.pack "123456789")
-- RunGet 4050765991979987505
getWordhost ::
  Get () Word
getWordhost =
  getPtr (sizeOf (undefined :: Word))
{-# INLINE getWordhost #-}

-- |
--
-- >>> runGet getWord16host (BLC.pack "abcde")
-- RunGet 25185
--
-- >>> runGet getWord16host (BLC.pack "12345")
-- RunGet 12849
getWord16host ::
  Get () Word16
getWord16host =
  getPtr (sizeOf (undefined :: Word16))
{-# INLINE getWord16host #-}

-- |
--
-- >>> runGet getWord32host (BLC.pack "abcde")
-- RunGet 1684234849
--
-- >>> runGet getWord32host (BLC.pack "12345")
-- RunGet 875770417
getWord32host ::
  Get () Word32
getWord32host =
  getPtr (sizeOf (undefined :: Word32))
{-# INLINE getWord32host #-}

-- |
--
-- >>> runGet getWord64host (BLC.pack "abcdeghi")
-- RunGet 7595434456733934177
--
-- >>> runGet getWord64host (BLC.pack "123456789")
-- RunGet 4050765991979987505
getWord64host ::
  Get () Word64
getWord64host =
  getPtr (sizeOf (undefined :: Word64))
{-# INLINE getWord64host #-}

------------------------------------------------------------------------
-- Unchecked shifts

shiftlW16 ::
  Word16
  -> Int
  -> Word16
shiftlW32 ::
  Word32
  -> Int
  -> Word32
shiftlW64 ::
  Word64
  -> Int
  -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftlW16 (W16# w) (I# i) =
  W16# (w `uncheckedShiftL#`   i)
shiftlW32 (W32# w) (I# i) =
  W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftlW64 (W64# w) (I# i) =
  W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
  uncheckedShiftL64# ::
    Word64#
    -> Int#
    -> Word64#
#endif

#else
shiftlW64 (W64# w) (I# i) =
  W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftlW16 =
  shiftL
shiftlW32 =
  shiftL
shiftlW64 =
  shiftL
#endif

----

data XDecoder e a =
  XFail !B.ByteString e
  | XPartial (Maybe B.ByteString -> XDecoder e a)
  | XDone !B.ByteString a
  | XBytesRead {-# UNPACK #-} !Int64 (Int64 -> XDecoder e a)

xdecoder :: 
  (B.ByteString -> e -> x)
  -> ((Maybe B.ByteString -> XDecoder e a) -> x)
  -> (B.ByteString -> a -> x)
  -> (Int64 -> (Int64 -> XDecoder e a) -> x)
  -> XDecoder e a
  -> x
xdecoder f _ _ _ (XFail b e) =
  f b e
xdecoder _ p _ _ (XPartial k) =
  p k
xdecoder _ _ d _ (XDone b a) =
  d b a
xdecoder _ _ _ r (XBytesRead i k) =
  r i k

_XFail ::
  Prism' (XDecoder e a) (B.ByteString, e)
_XFail =
  prism'
    (uncurry XFail)
    (\x -> case x of
             XFail b e -> Just (b, e)
             _ -> Nothing)

_XPartial ::
  Prism' (XDecoder e a) (Maybe B.ByteString -> XDecoder e a)
_XPartial =
  prism'
    XPartial
    (\x -> case x of
             XPartial k -> Just k
             _ -> Nothing)

_XDone ::
  Prism' (XDecoder e a) (B.ByteString, a)
_XDone =
  prism'
    (uncurry XDone)
    (\x -> case x of
             XDone b a -> Just (b, a)
             _ -> Nothing)
_XBytesRead ::
  Prism' (XDecoder e a) (Int64, Int64 -> XDecoder e a)
_XBytesRead =
  prism'
    (uncurry XBytesRead)
    (\x -> case x of
             XBytesRead i k -> Just (i, k)
             _ -> Nothing)

{-# INLINE _XFail #-}
{-# INLINE _XPartial #-}
{-# INLINE _XDone #-}
{-# INLINE _XBytesRead #-}

instance Functor (XDecoder e) where
  fmap =
    bimap id

instance Bifunctor XDecoder where
  bimap f _ (XFail b e) =
    XFail b (f e)
  bimap f g (XPartial k) =
    XPartial (bimap f g . k)
  bimap _ g (XDone b a) =
    XDone b (g a)
  bimap f g (XBytesRead i k) =
    XBytesRead i (bimap f g . k)
    
instance Bifoldable XDecoder where
  bifoldMap f _ (XFail _ e) =
    f e
  bifoldMap _ _ (XPartial _) =
    mempty
  bifoldMap _ g (XDone _ a) =
    g a
  bifoldMap _ _ (XBytesRead _ _) =
    mempty

instance Foldable (XDecoder e) where
  foldMap _ (XFail _ _) =
    mempty
  foldMap _ (XPartial _) =
    mempty
  foldMap f (XDone _ a) =
    f a
  foldMap _ (XBytesRead _ _) =
    mempty

data CompletedXDecoder e a =
  CompletedFail !B.ByteString e
  | CompletedDone !B.ByteString a
  deriving (Eq, Ord, Show)

completedXDecoder ::
  (B.ByteString -> e -> x)
  -> (B.ByteString -> a -> x)
  -> CompletedXDecoder e a
  -> x
completedXDecoder f _ (CompletedFail b e) =
  f b e
completedXDecoder _ d (CompletedDone b a) =
  d b a

_CompletedFail ::
  Prism' (CompletedXDecoder e a) (B.ByteString, e)
_CompletedFail =
  prism'
    (uncurry CompletedFail)
    (\x -> case x of
             CompletedFail b e -> Just (b, e)
             _ -> Nothing)

_CompletedDone ::
  Prism' (CompletedXDecoder e a) (B.ByteString, a)
_CompletedDone =
  prism'
    (uncurry CompletedDone)
    (\x -> case x of
             CompletedDone b a -> Just (b, a)
             _ -> Nothing)

completedIso ::
  Iso (CompletedXDecoder e a) (CompletedXDecoder f b) (Either e a, B.ByteString) (Either f b, B.ByteString)
completedIso =
  iso
    (\d -> case d of
             CompletedFail b e -> (Left e, b)
             CompletedDone b a -> (Right a, b))
    (\z -> case z of
             (Left e, b) -> CompletedFail b e
             (Right a, b) -> CompletedDone b a)
{-# INLINE completedIso #-}

completedByteString ::
  Lens' (CompletedXDecoder e a) B.ByteString
completedByteString =
  lens
    (\d -> case d of
             CompletedFail b _ -> b
             CompletedDone b _ -> b)
    (\d b -> case d of
               CompletedFail _ e -> CompletedFail b e
               CompletedDone _ a -> CompletedDone b a)
{-# INLINE completedByteString #-}

completedValue ::
  Lens' (CompletedXDecoder e a) (Either e a)
completedValue =
  lens
    (\d -> case d of
             CompletedFail _ e -> Left e
             CompletedDone _ a -> Right a)
    (\d z -> case d of
               CompletedFail b e -> CompletedFail b (either id (pure e) z)
               CompletedDone b a -> CompletedDone b (either (pure a) id z))
{-# INLINE completedValue #-}

instance Functor (CompletedXDecoder e) where
  fmap =
    bimap id

instance Bifunctor CompletedXDecoder where
  bimap f _ (CompletedFail b e) =
    CompletedFail b (f e)
  bimap _ g (CompletedDone b a) =
    CompletedDone b (g a)

instance Foldable (CompletedXDecoder e) where
  foldMap _ (CompletedFail _ _) =
    mempty
  foldMap f (CompletedDone _ a) =
    f a

instance Traversable (CompletedXDecoder e) where
  traverse = 
    bitraverse pure

instance Bifoldable CompletedXDecoder where
  bifoldMap f _ (CompletedFail _ e) = 
    f e
  bifoldMap _ g (CompletedDone _ a) =
    g a

instance Bitraversable CompletedXDecoder where
  bitraverse f _ (CompletedFail b e) =
    fmap (CompletedFail b) (f e)
  bitraverse _ g (CompletedDone b a) =
    fmap (CompletedDone b) (g a)

uncomplete ::
  Prism' (XDecoder e a) (CompletedXDecoder e a)
uncomplete =
  prism'
    (\d -> case d of
             CompletedFail b e -> XFail b e
             CompletedDone b a -> XDone b a)
    (\d -> case d of
             XFail b e -> Just (CompletedFail b e)
             XPartial _ -> Nothing
             XDone b a -> Just (CompletedDone b a)
             XBytesRead _ _ -> Nothing)
{-# INLINE uncomplete #-}

uncompletedByteString ::
  Traversal' (XDecoder e a) B.ByteString
uncompletedByteString = 
  uncomplete . completedByteString
{-# INLINE uncompletedByteString #-}

uncompletedValue ::
  Traversal' (XDecoder e a) (Either e a)
uncompletedValue = 
  uncomplete . completedValue
{-# INLINE uncompletedValue #-}
