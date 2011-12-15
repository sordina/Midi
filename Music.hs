module Music where

import Midi

data Letter = A | B | C | D | E | F | G deriving Show

data Item = Letter Letter | Sharp Item | Flat Item deriving Show

data Scale = Scale Item Mode deriving Show

data Mode = Major | Minor deriving Show
