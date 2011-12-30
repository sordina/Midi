{-# Language TupleSections #-}

import Midi
import qualified Data.ByteString as BS

main = BS.writeFile "test2.mid" $ make_music 4 4 $ Melody melody2

pairs = map (10,) melody

melody = take 4000 $ zipWith note prime_notes $ cycle (replicate 64 major ++ replicate 64 minor ++ replicate 64 aton )

melody2 = take 4000 $
  zipWith3 note2 (map (fromIntegral . (`mod` 7)) primes) [0..] $
    concat $
      map (replicate 32) $
        cycle [major, minor, aton]

prime_notes = map (`div` 2) primes

notes = concat . iterate (map (+12))

major = notes [0,2,4,5,7,9,11]
minor = map (subtract 8) $ notes [0,2,3,5,7,8,10]
aton  = map (subtract 12) $ notes [0,2..11]

note n s = s !! (n `mod` 8) + 60

note2 t n s = (t, note n s)

-- numeric sequences

primes = 2 : 3 : 5 : filter isPrime [7,9..]

isPrime n = all ((/=0).(mod n)) (takeWhile (<= s) primes)
  where
    s = floor $ sqrt $ fromIntegral n
