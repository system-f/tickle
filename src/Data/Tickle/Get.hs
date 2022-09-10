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
, lazyByteString
, lazyByteStringNul
, remainingLazyByteString
, ptr
, word8
, word16be
, word16le
, word32be
, word32le
, word64be
, word64le
, wordhost
, word16host
, word32host 
, word64host
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
  -- ** Higher-level parsers
  -- *** IEEE754 parsers
, toFloat
, toFloat16
, float16be
, float16le
, float32be
, float32le
, float64be
, float64le
  -- *** Integer parsers
, int8
, int16be
, int16le
, int32be
, int32le
, int64be
, int64le
, IntegerError
, integerError
, _IntegerTagUnexpectedEof
, _Integer0TagUnexpectedEof
, _Integer1TagUnexpectedEof
, _IntegerListError
, integer
  -- *** List parsers
, ListError
, listError
, listErrorIso
, list
, many
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
, runGetFile
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

import Data.Bits((.|.), (.&.), shiftL, shiftR)
import qualified Data.ByteString as B(ByteString, concat, append, length, splitAt, empty, null, break, drop)
import qualified Data.ByteString.Lazy as L(ByteString, toChunks, fromChunks, readFile)
import qualified Data.ByteString.Lazy.Internal as LI(ByteString(Chunk, Empty))
import qualified Data.ByteString.Unsafe as BU(unsafeDrop, unsafeTake, unsafeHead, unsafeIndex, unsafeUseAsCString)
import qualified Data.Functor.Alt as Al(Alt(some, many))
import Data.Tickle.IsolateError(IsolateError, _NegativeSize, _IsolateXFail, _UnexpectedConsumed)
import Data.Tickle.RunGetResult(RunGetResult, _RunGet, _RunGetFail)
import Foreign(Ptr, castPtr, Storable(peek), sizeOf, alloca, poke)
import System.IO.Unsafe(unsafePerformIO)
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Word(uncheckedShiftL64#, Word8, Word16(W16#), Word32(W32#), Word64(W64#))
import GHC.Base(uncheckedShiftLWord16#, uncheckedShiftLWord32#, Int(I#))
#endif
import Prelude(($!), undefined, seq)
import Papa hiding (many, (.>>), (<<.))

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
-- >>> runGet (bimap (const True) (\x -> x + x) word8) (BLC.pack "")
-- RunGetFail 0 True
--
-- >>> runGet (bimap (const True) (\x -> x + x) word8) (BLC.pack "abc")
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
-- >>> runGet (fmap (\x -> x + x) word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (\x -> x + x) word8) (BLC.pack "abc")
-- RunGet 194
instance Functor (Get e) where
  fmap =
    fmapG

-- | Apply a function on the @Get@ decoder result.
--
-- >>> runGet (fmap (+) word8 <.> word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (+) word8 <.> word8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (fmap (+) word8 <.> word8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (fmap (+) word8 <.> word8) (BLC.pack "abc")
-- RunGet 195
instance Apply (Get e) where
  (<.>) =
    (<*>)

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
-- >>> runGet (fmap (+) word8 <*> word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (fmap (+) word8 <*> word8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (fmap (+) word8 <*> word8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (fmap (+) word8 <*> word8) (BLC.pack "abc")
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
-- >>> runGet (word8 >>- \c1 -> fmap (\c2 -> c1 + c2) word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (word8 >>- \c1 -> fmap (\c2 -> c1 + c2) word8) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (word8 >>- \c1 -> fmap (\c2 -> c1 + c2) word8) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (word8 >>- \c1 -> fmap (\c2 -> c1 + c2) word8) (BLC.pack "abc")
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
-- >>> runGet (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2)) (BLC.pack "a")
-- RunGetFail 1 ()
--
-- >>> runGet (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2)) (BLC.pack "ab")
-- RunGet 195
--
-- >>> runGet (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2)) (BLC.pack "abc")
-- RunGet 195
instance Monad (Get e) where
  return =
    returnG
  (>>=) =
    bindG

-- | Pick between two @Get@ decoders, finding the first to not fail.
--
-- >>> runGet ((+1) <$> word8 <!> subtract 1 <$> word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet ((+1) <$> word8 <!> subtract 1 <$> word8) (BLC.pack "abc")
-- RunGet 98
--
-- >>> runGet (word8 <!> failGet ()) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (word8 <!> failGet ()) (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet (Al.some word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (Al.some word8) (BLC.pack "a")
-- RunGet [97]
--
-- >>> runGet (Al.some word8) (BLC.pack "abc")
-- RunGet [97,98,99]
--
-- >>> runGet (Al.many word8) (BLC.pack "")
-- RunGet []
--
-- >>> runGet (Al.many word8) (BLC.pack "a")
-- RunGet [97]
--
-- >>> runGet (Al.many word8) (BLC.pack "abc")
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
-- >>> runGet (((+1) <$> word8) <> (subtract 1 <$> word8)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (((+1) <$> word8) <> (subtract 1 <$> word8)) (BLC.pack "abc")
-- RunGet 98
--
-- >>> runGet (word8 <> failGet ()) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (word8 <> failGet ()) (BLC.pack "abc")
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
-- >>> runGet (runAndKeepTrack word8 :: Get () (CompletedXDecoder () Word8, [BC.ByteString])) (BLC.pack "")
-- RunGet (CompletedFail "" (),[])
--
-- >>> runGet (runAndKeepTrack word8 :: Get () (CompletedXDecoder () Word8, [BC.ByteString])) (BLC.pack "abc")
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
-- >>> runGet (word8 >> word16be >> word32le >> bytesRead) (BLC.pack "abcdefghijk")
-- RunGet 7
bytesRead ::
  Get e Int64
bytesRead =
  Get (\b q -> XBytesRead (fromIntegral (B.length b)) (q b))

-- |
--
-- >>> runGet (isolate 1 word8) (BLC.pack "ab")
-- RunGet 97
--
-- >>> runGet (isolate 1 word8) (BLC.pack "abcde")
-- RunGet 97
--
-- >>> runGet (isolate 2 word16le) (BLC.pack "abcde")
-- RunGet 25185
--
-- >>> runGet (isolate 1 word16le) (BLC.pack "abcde")
-- RunGetFail 0 (IsolateXFail ())
--
-- >>> runGet (isolate (-3) word16le) (BLC.pack "abcde")
-- RunGetFail 0 NegativeSize
--
-- >>> runGet (isolate 3 word16le) (BLC.pack "abcde")
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
                                   let (j, h) = B.splitAt n t
                                   in q h (Just j)
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
-- >>> runGet (word8 >>= \c -> skip 2 >> word8 >>= \d -> return (c,d)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (word8 >>= \c -> skip 2 >> word8 >>= \d -> return (c,d)) (BLC.pack "abcdefghi")
-- RunGet (97,100)
--
-- >>> runGet (word8 >>= \c -> skip 2 >> word8 >>= \d -> return (c,d)) (BLC.pack "abc")
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
-- >>> runGet (isNotEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "abc")
-- RunGet (97,True)
--
-- >>> runGet (isNotEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (isNotEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "a")
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
-- >>> runGet (isEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "abc")
-- RunGet (97,False)
--
-- >>> runGet (isEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (isEmpty >>= \p -> word8 >>= \w -> return (w, p)) (BLC.pack "a")
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
-- >>> runGet (lookAhead word8) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (lookAhead word8) (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet (lookAhead word8) (BLC.pack "a")
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
-- >>> runGet (lookAheadM (word8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "abc")
-- RunGet Nothing
--
-- >>> runGet (lookAheadM (word8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "bc")
-- RunGet (Just 103)
--
-- >>> runGet (lookAheadM (word8 >>= \w -> return (if even w then Just (w + 5) else Nothing))) (BLC.pack "")
-- RunGetFail 0 ()
lookAheadM ::
  Get e (Maybe a)
  -> Get e (Maybe a)
lookAheadM g =
  let g' = fmap (maybe (Left ()) Right) g
  in fmap (either (return Nothing) Just) (lookAheadE g')

-- |
--
-- >>> runGet (lookAheadE (word8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "abc")
-- RunGet (Right 93)
--
-- >>> runGet (lookAheadE (word8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "bc")
-- RunGet (Left 103)
--
-- >>> runGet (lookAheadE (word8 >>= \w -> return (if even w then Left (w + 5) else Right (w - 4)))) (BLC.pack "")
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
-- >>> runGet ([(), ()] !+ (setLabel [] word8)) (BLC.pack "")
-- RunGetFail 0 [(),()]
--
-- >>> runGet ([(), ()] !+ (setLabel [] word8)) (BLC.pack "abc")
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
-- >>> runGet ([(), ()] `addLabel` (setLabel [] word8)) (BLC.pack "")
-- RunGetFail 0 [(),()]
--
-- >>> runGet ([(), ()] `addLabel` (setLabel [] word8)) (BLC.pack "abc")
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
-- >>> runGet ("error" !- word8) (BLC.pack "")
-- RunGetFail 0 "error"
--
-- >>> runGet ("error" !- word8) (BLC.pack "abc")
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
-- >>> runGet ("error" `setLabel` word8) (BLC.pack "")
-- RunGetFail 0 "error"
--
-- >>> runGet ("error" `setLabel` word8) (BLC.pack "abc")
-- RunGet 97
setLabel ::
  e
  -> Get d a
  -> Get e a
setLabel =
  modifyLabel . return

-- |
--
-- >>> runGet (reverse !!- setLabel "error" word8) (BLC.pack "")
-- RunGetFail 0 "rorre"
--
-- >>> runGet (reverse !!- setLabel "error" word8) (BLC.pack "abc")
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
-- >>> runGet (reverse `modifyLabel` setLabel "error" word8) (BLC.pack "")
-- RunGetFail 0 "rorre"
--
-- >>> runGet (reverse `modifyLabel` setLabel "error" word8) (BLC.pack "abc")
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

toFloat ::
  (Storable w, Storable f) =>
  w
  -> f
toFloat w =
  unsafePerformIO (alloca (\buf ->
    do poke (castPtr buf) w
       peek buf))
{-# INLINE toFloat #-}

toFloat16 ::
  Word16
  -> Float
toFloat16 word16 = 
  let sign16 =
        word16 .&. 0x8000
      exp16 =
        word16 .&. 0x7C00
      frac16 =
        word16 .&. 0x3FF
      sign32 =
        if sign16 > 0
          then
            0x80000000 -- -0.0
            
          else
            0
      word32
        :: Word32
      word32 | word16 .&. 0x7FFF == 0 =
        0
             | exp16 == 0x7C00 =
        special
             | otherwise =
        shiftL exp32 23 .|. shiftL frac32 13
      special =
        if frac16 == 0
          -- Infinity
          then 0x7F800000
          
          -- NaN; signals are maintained in lower 10 bits
          else 0x7FC00000 .|. fromIntegral frac16
      (exp32, frac32) =
        if exp16 > 0
          then normalised
          else denormalised
      normalised =
        let xp = (fromIntegral exp16 `shiftR` 10) - 15 + 127
            frac = fromIntegral frac16
        in (xp, frac)
      denormalised =
        let xp = (fromIntegral exp16 `shiftR` 10) - 15 + 127 - e
            (e, frac ) = 
              let step acc x = if x .&. 0x400 == 0
                    then step (acc + 1) (shiftL x 1)
                    else (acc, fromIntegral x .&. 0x3FF)
              in step 0 (shiftL frac16 1) 
        in (xp, frac)
  in toFloat (sign32 .|. word32) 
{-# INLINE toFloat16 #-}

float16be ::
  Get () Float
float16be =
  fmap toFloat16 word16be
{-# INLINE float16be #-}

float16le ::
  Get () Float
float16le =
  fmap toFloat16 word16le
{-# INLINE float16le #-}

float32be ::
  Get () Float
float32be =
  fmap toFloat word32be
{-# INLINE float32be #-}

float32le ::
  Get () Float
float32le =
  fmap toFloat word32le
{-# INLINE float32le #-}

float64be ::
  Get () Double
float64be =
  fmap toFloat word64be
{-# INLINE float64be #-}

float64le ::
  Get () Double
float64le =
  fmap toFloat word64le
{-# INLINE float64le #-}

int8 ::
  Get () Int8
int8 =
  fmap fromIntegral word8
{-# INLINE int8 #-}

int16be ::
  Get () Int16
int16be =
  fmap fromIntegral word16be
{-# INLINE int16be #-}

int16le ::
  Get () Int16
int16le =
  fmap fromIntegral word16le
{-# INLINE int16le #-}

int32be ::
  Get () Int32
int32be =
  fmap fromIntegral word32be
{-# INLINE int32be #-}

int32le ::
  Get () Int32
int32le =
  fmap fromIntegral word32le
{-# INLINE int32le #-}

int64be ::
  Get () Int64
int64be =
  fmap fromIntegral word64be
{-# INLINE int64be #-}

int64le ::
  Get () Int64
int64le =
  fmap fromIntegral word64le
{-# INLINE int64le #-}

data IntegerError =
  IntegerTagUnexpectedEof
  | Integer0TagUnexpectedEof Word8
  | Integer1TagUnexpectedEof 
  | IntegerListError ListError
  deriving (Eq, Ord, Show)

integerError :: 
  a
  -> (Word8 -> a)
  -> a
  -> (ListError -> a)
  -> IntegerError
  -> a
integerError u _ _ _ IntegerTagUnexpectedEof =
  u
integerError _ u _ _ (Integer0TagUnexpectedEof w) =
  u w
integerError _ _ u _ Integer1TagUnexpectedEof =
  u
integerError _ _ _ u (IntegerListError e) =
  u e

_IntegerTagUnexpectedEof ::
  Prism' IntegerError ()
_IntegerTagUnexpectedEof =
  prism'
    (\() -> IntegerTagUnexpectedEof)
    (\x -> case x of
             IntegerTagUnexpectedEof ->
               Just ()
             _ ->
               Nothing)

_Integer0TagUnexpectedEof ::
  Prism' IntegerError Word8
_Integer0TagUnexpectedEof =
  prism'
    Integer0TagUnexpectedEof
    (\x -> case x of
             Integer0TagUnexpectedEof w ->
               Just w
             _ ->
               Nothing)

_Integer1TagUnexpectedEof ::
  Prism' IntegerError ()
_Integer1TagUnexpectedEof =
  prism'
    (\() -> Integer1TagUnexpectedEof)
    (\x -> case x of
             Integer1TagUnexpectedEof ->
               Just ()
             _ ->
               Nothing)

_IntegerListError ::
  Prism' IntegerError ListError
_IntegerListError =
  prism'
    IntegerListError
    (\x -> case x of
             IntegerListError e ->
               Just e
             _ ->
               Nothing)

integer ::
  Get IntegerError Integer
integer =
  do t <- IntegerTagUnexpectedEof !- word8
     case t of
       0 ->
         Integer0TagUnexpectedEof t !- fmap fromIntegral int32be
       _ -> 
         do s <- Integer1TagUnexpectedEof !- word8
            y <- IntegerListError !!- list word8
            let v = foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0 y
            return $! if s == (1 :: Word8) then v else - v

data ListError =
  ListUnexpectedEof
  | ListTagError
  deriving (Eq, Ord, Show)

listError ::
  a
  -> a
  -> ListError
  -> a
listError u _ ListUnexpectedEof =
  u
listError _ e ListTagError =
  e

listErrorIso ::
  Iso' Bool ListError
listErrorIso =
  iso
    (\p -> if p then ListUnexpectedEof else ListTagError)
    (== ListUnexpectedEof)

list ::
  Get e a
  -> Get ListError [a]
list q =
  do n <- ListTagError !- int64be
     ListUnexpectedEof !- many q n

many ::
  Get e a
  -> Int64
  -> Get e [a]
many g n =
  let go x 0 =
        return $! reverse x
      go x i =
        do a <- g
           x `seq` go (a:x) (i - 1)
  in go [] n

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
  readN n (\s -> unsafePerformIO (BU.unsafeUseAsCString s (f . castPtr)))
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
-- >>> (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2)) .>> BLC.pack "abc"
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
-- >>> BLC.pack "abc" <<. (word8 >>= \c1 -> word8 >>= \c2 -> return (c1 + c2))
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
    
runGetFile ::
  Get e a
  -> FilePath
  -> IO (RunGetResult e a)
runGetFile g =
  fmap (runGet g) . L.readFile

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
-- >>> runGet (lazyByteString 5) (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet (lazyByteString 5) (BLC.pack "abc")
-- RunGetFail 3 ()
--
-- >>> runGet (lazyByteString 5) (BLC.pack "abcdefg")
-- RunGet "abcde"
lazyByteString ::
  Int64
  -> Get () L.ByteString
lazyByteString =
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
-- >>> runGet lazyByteStringNul (BLC.pack "")
-- RunGetFail 0 ()
--
-- >>> runGet lazyByteStringNul (BLC.pack "abc")
-- RunGetFail 3 ()
--
-- >>> runGet lazyByteStringNul (BLC.pack "abc\0")
-- RunGet "abc"
--
-- >>> runGet lazyByteStringNul (BLC.pack "abc\0def")
-- RunGet "abc"
lazyByteStringNul ::
  Get () L.ByteString
lazyByteStringNul =
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
-- >>> runGet remainingLazyByteString  (BLC.pack "")
-- RunGet ""
--
-- >>> runGet remainingLazyByteString  (BLC.pack "abc")
-- RunGet "abc"
remainingLazyByteString ::
  Get e L.ByteString
remainingLazyByteString =
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

ptr ::
  Storable a =>
  Int
  -> Get () a
ptr n =
  readNWith n peek
{-# INLINE ptr #-}

{-# RULES

"word8/readN" word8 =
  readN 1 BU.unsafeHead
  
"word16be/readN" word16be =
  readN 2 word16be'

"word16le/readN" word16le =
  readN 2 word16le'

"word32be/readN" word32be =
  readN 4 word32be'

"word32le/readN" word32le =
  readN 4 word32le'

"word64be/readN" word64be =
  readN 8 word64be'

"word64le/readN" word64le =
  readN 8 word64le'

  #-}

-- |
--
-- >>> runGet word8 (BLC.pack "abc")
-- RunGet 97
--
-- >>> runGet word8 (BLC.pack "123")
-- RunGet 49
word8 ::
  Get () Word8
word8 =
  readN 1 BU.unsafeHead
{-# INLINE [0] word8 #-}

word16be' ::
  B.ByteString
  -> Word16
word16be' s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW16` 8) .|.
    fromIntegral (s `BU.unsafeIndex` 1)
{-# INLINE word16be' #-}

-- |
--
-- >>> runGet word16be (BLC.pack "abc")
-- RunGet 24930
--
-- >>> runGet word16be (BLC.pack "123")
-- RunGet 12594
word16be ::
  Get () Word16
word16be =
  readN 2 word16be'
{-# INLINE [0] word16be #-}

word16le' ::
  B.ByteString
  -> Word16
word16le' s =
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW16` 8) .|.
    fromIntegral (s `BU.unsafeIndex` 0)
{-# INLINE word16le' #-}

-- |
--
-- >>> runGet word16le (BLC.pack "abc")
-- RunGet 25185
--
-- >>> runGet word16le (BLC.pack "123")
-- RunGet 12849
word16le ::
  Get () Word16
word16le =
  readN 2 word16le'
{-# INLINE [0] word16le #-}

word32be' ::
  B.ByteString
  -> Word32
word32be' s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW32` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW32` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW32`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 3)
{-# INLINE word32be' #-}

-- |
--
-- >>> runGet word32be (BLC.pack "abcdef")
-- RunGet 1633837924
--
-- >>> runGet word32be (BLC.pack "123456")
-- RunGet 825373492
word32be ::
  Get () Word32
word32be =
  readN 4 word32be'
{-# INLINE [0] word32be #-}

word32le' ::
  B.ByteString
  -> Word32
word32le' s =
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW32` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW32` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW32`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 0)
{-# INLINE word32le' #-}

-- |
--
-- -- >>> runGet word32le (BLC.pack "abcdef")
-- RunGet 1684234849
--
-- >>> runGet word32le (BLC.pack "123456")
-- RunGet 875770417
word32le ::
  Get () Word32
word32le =
  readN 4 word32le'
{-# INLINE [0] word32le #-}

word64be' ::
  B.ByteString
  -> Word64
word64be' s =
    (fromIntegral (s `BU.unsafeIndex` 0) `shiftlW64` 56) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW64` 48) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW64` 40) .|.
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW64` 32) .|.
    (fromIntegral (s `BU.unsafeIndex` 4) `shiftlW64` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 5) `shiftlW64` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 6) `shiftlW64`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 7)
{-# INLINE word64be' #-}

-- |
--
-- >>> runGet word64be (BLC.pack "abcdefghi")
-- RunGet 7017280452245743464
--
-- >>> runGet word64be (BLC.pack "123456789")
-- RunGet 3544952156018063160
word64be ::
  Get () Word64
word64be =
  readN 8 word64be'
{-# INLINE [0] word64be #-}

word64le' ::
  B.ByteString
  -> Word64
word64le' s =
    (fromIntegral (s `BU.unsafeIndex` 7) `shiftlW64` 56) .|.
    (fromIntegral (s `BU.unsafeIndex` 6) `shiftlW64` 48) .|.
    (fromIntegral (s `BU.unsafeIndex` 5) `shiftlW64` 40) .|.
    (fromIntegral (s `BU.unsafeIndex` 4) `shiftlW64` 32) .|.
    (fromIntegral (s `BU.unsafeIndex` 3) `shiftlW64` 24) .|.
    (fromIntegral (s `BU.unsafeIndex` 2) `shiftlW64` 16) .|.
    (fromIntegral (s `BU.unsafeIndex` 1) `shiftlW64`  8) .|.
    fromIntegral (s `BU.unsafeIndex` 0) 
{-# INLINE word64le' #-}

-- |
--
-- >>> runGet word64le (BLC.pack "abcdefghi")
-- RunGet 7523094288207667809
--
-- >>> runGet word64le (BLC.pack "123456789")
-- RunGet 4050765991979987505
word64le ::
  Get () Word64
word64le =
  readN 8 word64le'
{-# INLINE [0] word64le #-}

-- |
--
-- >>> runGet wordhost (BLC.pack "abcdefghi")
-- RunGet 7523094288207667809
--
-- >>> runGet wordhost (BLC.pack "123456789")
-- RunGet 4050765991979987505
wordhost ::
  Get () Word
wordhost =
  ptr (sizeOf (undefined :: Word))
{-# INLINE wordhost #-}

-- |
--
-- >>> runGet word16host (BLC.pack "abcde")
-- RunGet 25185
--
-- >>> runGet word16host (BLC.pack "12345")
-- RunGet 12849
word16host ::
  Get () Word16
word16host =
  ptr (sizeOf (undefined :: Word16))
{-# INLINE word16host #-}

-- |
--
-- >>> runGet word32host (BLC.pack "abcde")
-- RunGet 1684234849
--
-- >>> runGet word32host (BLC.pack "12345")
-- RunGet 875770417
word32host ::
  Get () Word32
word32host =
  ptr (sizeOf (undefined :: Word32))
{-# INLINE word32host #-}

-- |
--
-- >>> runGet word64host (BLC.pack "abcdeghi")
-- RunGet 7595434456733934177
--
-- >>> runGet word64host (BLC.pack "123456789")
-- RunGet 4050765991979987505
word64host ::
  Get () Word64
word64host =
  ptr (sizeOf (undefined :: Word64))
{-# INLINE word64host #-}

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
  W16# (w `uncheckedShiftLWord16#`   i)
shiftlW32 (W32# w) (I# i) =
  W32# (w `uncheckedShiftLWord32#`   i)

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
  W64# (w `uncheckedShiftL64#` i)
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
