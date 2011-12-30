# Midi Music


This project aims to create a dead-simple interface for programatically outputting simple musical ideas
to Midi from Haskell with no dependencies other than ByteString and some core libraries such as Control.Arrow.


So far, this doesn't include any musical concepts other than

* Notes
* Melodies
* Chords
* Sequences of Items
* Parallel Items

There aren't even any rests or dynamics as of yet.

The reason behind this is that the project is designed to funciton as a
scratch-pad for musical ideas, and not get muddled down in details.

## A simple example:

```haskell
import Midi
import Data.List (tails)
import Data.List.Split
import qualified Data.ByteString as BS (writeFile)

main      = musicFile song

song      = Higher (-20) $ Sequence $ take 40 $ concatMap triads fiths
musicFile = BS.writeFile "Foo3.mid" . make_music 1 2
minor     = concat . iterate (map (Higher 12)) $ [A,B,C,D,E,F,G]
fiths     = iterate (drop 4) minor
triads    = take 8 . map ( Parallel . take 3 . every 3 ) . tails
every n   = map head . splitEvery n
```
