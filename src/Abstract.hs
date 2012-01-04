module Abstract (

  chromatic,
  atonal,
  triad,
  quadrad,
  nrad,
  major,
  minor_nat,
  minor_mel,
  pentatonic

) where


import Music
import Util (every, drops)
import GHC.Word (Word8)

chromatic :: Music a -> [ Music a ]
chromatic = iterate Sharp

atonal :: Music a -> [ Music a ]
atonal = every 2 . chromatic

nrad :: Int -> [ Music a ] -> [ Music a ]
nrad n = take n . every 2

triad :: [ Music a ] -> [ Music a ]
triad = nrad 3

quadrad :: [ Music a ] -> [ Music a ]
quadrad = nrad 4

major :: [ Music a ] -> [ Music a ]
major = drops major_drops

minor_nat :: [ Music a ] -> [ Music a ]
minor_nat = drops minor_drops

minor_mel :: [ Music a ] -> [ Music a ]
minor_mel = zipWith Higher mel_tweaks . minor_nat

pentatonic :: [ Music a ] -> [ Music a ]
pentatonic = drops pentatonic_drops

mel_tweaks :: [ Word8 ]
mel_tweaks = cycle [0,0,0,0,0,0,1]

major_drops :: [ Int ]
major_drops = cycle [2,2,1,2,2,2,1]

minor_drops :: [ Int ]
minor_drops = drop 5 major_drops

pentatonic_drops :: [ Int ]
pentatonic_drops = cycle [2,2,3,2,3]
