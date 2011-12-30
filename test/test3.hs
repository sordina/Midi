import Data.List.Split
import Midi

main = write_music "test3.mid" 1 1 song

song    = Parallel [under, over]
under   = Higher (-24) $ Longer 2 $ Longer 2 $ Sequence [ Sequence chord, Longer 4 (Parallel chord) ]
over    = Sequence $ replicate 2 $ Sequence $ take 8 major
chord   = take 4 $ every 2 major
minor   = concat $ iterate (map oct) [A,B,C,D,E,F,G]
major   = drop 2 $ minor
oct     = Higher 12
every n = map head . splitEvery n
