import Midi
import Abstract
import Prelude hiding (min)
import Data.List.Split (splitEvery)

main = Midi.write_music "test5.mid" 2 1 song

song = Sequence [ accompany
                , Parallel [ accompany
                           , evolve melody ]
                , Longer 8 A]

accompany = Longer 2 $ lower 12 $ evolve chords

evolve n = Sequence $ take 8 $ map (flip Higher n . restrict_range) [ 0, 4.. ]

-- http://www.reddit.com/r/musictheory/comments/o240v/most_beautiful_progressionschords/c3dqs1e
progression = [ min A
              , maj C
              , maj G
              , maj F ]

durations = cycle [ 3, 5 ]

chords = Sequence $ zipWith Longer durations (map (Parallel . nrad 5) progression)

melody = Sequence $ concat $ zipWith grab durations (map (drop 2 . invert) progression)

restrict_range = flip mod 13

maj = major . chromatic
min = minor_nat . chromatic

lower = Higher . negate

-- Amin, Cmaj, Gmaj, Fmaj

grab = take . (*2) . fromIntegral

invert = concatMap reverse . splitEvery 8
