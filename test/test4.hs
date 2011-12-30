import Midi
import System.Random

main = write song

write = write_music "test4.mid" 6 1

gen = mkStdGen 0 -- Fixed Seed for consistency

theme = Parallel [
      long'' (low C)
    , Sequence [
        long (low (low C)),
        long B,
        long' A
      ]
  ]

conclusion = Parallel [
      long'' (low' C)
    , long'' (low C)
    , Sequence [
        long'' B,
        long'' A
      ]
  ]

melody_notes   = [ A, B, C, D, E ]
melody_pitches = map (melody_notes !!) $ randomRs (0, length melody_notes - 1) gen
melody_lengths = randomRs (1, 6) gen
melody = high $ Sequence $ take 36 $ zipWith Longer melody_lengths melody_pitches

descending_theme = take 6 $ iterate (lower 2) theme

descending_conclusion = take 3 $ iterate (lower 3) conclusion

song = Longer 6 $ Sequence [
    acompaniment 1
  , Parallel [
    acompaniment 2,
    melody
    ]
  ]

acompaniment n = Sequence $ concat $ take n $ repeat $ descending_theme ++ descending_conclusion

long   = Longer 2
long'  = long . long
long'' = long . long . long

low   = Higher (-12)
low'  = low . low
low'' = low' . low

high = Higher 12

lower n = Higher (0-n)
