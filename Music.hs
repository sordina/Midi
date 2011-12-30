{-# Language GADTs, DeriveFunctor #-}

module Music (

    Music(..),
    events

  ) where

import TimeSet
import GHC.Word
import Control.Arrow (first, second, (>>>))

data Music a where
  Melody   :: [a]         -> Music a
  Chord    :: [a]         -> Music a
  Lone     :: a           -> Music a
  Pair     :: (a, a)      -> Music a
  Tripple  :: (a, a, a)   -> Music a
  Parallel :: [Music a]   -> Music a
  Sequence :: [Music a]   -> Music a

  Sharp    :: Music a     -> Music a
  Flat     :: Music a     -> Music a

  Higher   :: Word8       -> Music a -> Music a
  Longer   :: Word8       -> Music a -> Music a

  A_       :: Music a
  A        :: Music a
  A'       :: Music a
  B_       :: Music a
  B        :: Music a
  C        :: Music a
  C'       :: Music a
  D_       :: Music a
  D        :: Music a
  D'       :: Music a
  E_       :: Music a
  E        :: Music a
  F        :: Music a
  F'       :: Music a
  G_       :: Music a
  G        :: Music a
  G'       :: Music a

  deriving (Show, Eq, Ord, Functor)

data SimpleMusic a = Seq [SimpleMusic a] | Par [SimpleMusic a] | Mel [a]

type Note       = (Word8, Word8) -- Pitch, Length
data Pitch      = On Word8    | Off Word8 deriving (Ord, Eq, Show)
type PitchEvent = (Word8, Pitch)
data NoteEvent  = NoteOn Note | NoteOff Note deriving (Ord, Eq, Show)

simplify :: Music Note -> SimpleMusic Note
simplify (Melody m) = Mel m
simplify (Chord  c) = Par $ map (Mel . return) c
simplify (Lone   n) = Mel [n]
simplify (Pair (a,b)) = Par [Mel [a], Mel [b]]
simplify (Tripple (a,b,c)) = Par [Mel [a], Mel [b], Mel [c]]
simplify (Parallel m) = Par $ map simplify m
simplify (Sequence m) = Seq $ map simplify m
simplify (Sharp m) = raise 1 $ simplify m
simplify (Flat m) = raise (-1) $ simplify m
simplify (Higher n m) = raise n $ simplify m
simplify (Longer n m) = lengthen n $ simplify m

simplify A_ = pitch 1
simplify A  = pitch 2
simplify A' = pitch 3
simplify B_ = pitch 4
simplify B  = pitch 5
simplify C  = pitch 6
simplify C' = pitch 7
simplify D_ = pitch 8
simplify D  = pitch 9
simplify D' = pitch 10
simplify E_ = pitch 11
simplify E  = pitch 12
simplify F  = pitch 13
simplify F' = pitch 14
simplify G_ = pitch 15
simplify G  = pitch 16
simplify G' = pitch 17

pitch n = Mel [(1, n+60)] -- Don't start pitches at 0...

pitchEventify :: Note -> [PitchEvent]
pitchEventify (a,b) = [(0, On a), (b, Off a)]

noteEventify :: PitchEvent -> NoteEvent
noteEventify (a, On  n) = NoteOn  (n, a)
noteEventify (a, Off n) = NoteOff (n, a)

musicEventify :: SimpleMusic Note -> SimpleMusic PitchEvent
musicEventify (Seq m) = Seq . map musicEventify $ m
musicEventify (Par m) = Par . map musicEventify $ m
musicEventify (Mel m) = Mel $ concatMap pitchEventify m

raise :: Word8 -> SimpleMusic Note -> SimpleMusic Note
raise n (Seq m) = Seq . map (raise n) $ m
raise n (Par m) = Par . map (raise n) $ m
raise n (Mel m) = Mel $ map (first (+n)) m

lengthen :: Word8 -> SimpleMusic Note -> SimpleMusic Note
lengthen n (Seq m) = Seq . map (lengthen n) $ m
lengthen n (Par m) = Par . map (lengthen n) $ m
lengthen n (Mel m) = Mel $ map (second (*n)) m

sequentialize :: SimpleMusic PitchEvent -> [PitchEvent]
sequentialize (Seq m) = concatMap sequentialize m
sequentialize (Par m) = normalize $ map sequentialize m
sequentialize (Mel m) = m

-- timing

events :: Music Note -> [NoteEvent]
events = simplify >>> musicEventify >>> sequentialize >>> map noteEventify
