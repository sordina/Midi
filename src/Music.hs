{-# Language GADTs, DeriveFunctor #-}

module Music (

    Music(..),
    Note,
    Pitch(..),
    PitchEvent,
    events

  ) where

import Midi.TimeSet
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
  Longer   :: Integer     -> Music a -> Music a

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

type Note       = (Integer, Word8) -- Length, Pitch
data Pitch      = On Word8    | Off Word8 deriving (Ord, Eq, Show)
type PitchEvent = (Integer, Pitch)

simplify :: Music Note -> SimpleMusic Note

simplify (Melody m)         = Mel m
simplify (Chord  c)         = Par $ map (Mel . return) c
simplify (Lone   n)         = Mel [n]
simplify (Pair (a,b))       = Par [Mel [a], Mel [b]]
simplify (Tripple (a,b,c))  = Par [Mel [a], Mel [b], Mel [c]]
simplify (Parallel m)       = Par $ map simplify m
simplify (Sequence m)       = Seq $ map simplify m
simplify (Sharp m)          = raise 1 $ simplify m
simplify (Flat m)           = raise (-1) $ simplify m
simplify (Higher n m)       = raise n $ simplify m
simplify (Longer n m)       = lengthen n $ simplify m

simplify A_ = pitch (-4)
simplify A  = pitch (-3)
simplify A' = pitch (-2)
simplify B_ = pitch (-2)
simplify B  = pitch (-1)
simplify C  = pitch 0
simplify C' = pitch 1
simplify D_ = pitch 1
simplify D  = pitch 2
simplify D' = pitch 3
simplify E_ = pitch 3
simplify E  = pitch 4
simplify F  = pitch 5
simplify F' = pitch 6
simplify G_ = pitch 6
simplify G  = pitch 7
simplify G' = pitch 9

pitch :: Word8 -> SimpleMusic Note
pitch n = Mel [(1, n+60)] -- Don't start pitches at 0...

pitchEventify :: Note -> [PitchEvent]
pitchEventify (a,b) = [(0, On b), (a, Off b)]

musicEventify :: SimpleMusic Note -> SimpleMusic PitchEvent
musicEventify (Seq m) = Seq . map musicEventify $ m
musicEventify (Par m) = Par . map musicEventify $ m
musicEventify (Mel m) = Mel $ concatMap pitchEventify m

raise :: Word8 -> SimpleMusic Note -> SimpleMusic Note
raise n (Seq m) = Seq . map (raise n) $ m
raise n (Par m) = Par . map (raise n) $ m
raise n (Mel m) = Mel $ map (second (+n)) m

lengthen :: Integer -> SimpleMusic Note -> SimpleMusic Note
lengthen n (Seq m) = Seq . map (lengthen n) $ m
lengthen n (Par m) = Par . map (lengthen n) $ m
lengthen n (Mel m) = Mel $ map (first (*n)) m

sequentialize :: SimpleMusic PitchEvent -> [PitchEvent]
sequentialize (Seq m) = concatMap sequentialize m
sequentialize (Par m) = normalize $ map sequentialize m
sequentialize (Mel m) = m

-- timing

events :: Music Note -> [PitchEvent]
events = simplify >>> musicEventify >>> sequentialize
