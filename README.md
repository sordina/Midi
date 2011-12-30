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
import Data.List.Split
import Midi

--                 FilePath,    Half-Beats-Per-Second,  EOT-Delay,  Music
main = write_music "test3.mid"  1                       1           song

song    = Parallel [under, over]
under   = Higher (-24) $ Longer 2 $ Longer 2 $ Sequence [ Sequence chord, Longer 4 (Parallel chord) ]
over    = Sequence $ replicate 2 $ Sequence $ take 8 major
chord   = take 4 $ every 2 major
minor   = concat $ iterate (map oct) [A,B,C,D,E,F,G]
major   = drop 2 $ minor
oct     = Higher 12
every n = map head . splitEvery n
```

Outout: [MIDI Download](https://github.com/sordina/Midi/blob/master/examples/test3.mid?raw=true)
