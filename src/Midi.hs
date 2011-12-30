{-# Language OverloadedStrings #-}

module Midi (

  make_music,
  write_music,
  Note,
  Music(..),

) where

-- Taken from http://www.sonicspot.com/guide/midifiles.html

import Data.ByteString.Char8 ()
import Data.ByteString       (ByteString, pack, concat, append )
import GHC.Word              (Word8)
import Prelude hiding (concat)
import qualified Data.ByteString as BS

import Music

write_music :: FilePath -> Word8 -> Integer -> Music Note -> IO ()
write_music path dpb eot_delay = BS.writeFile path . make_music dpb eot_delay

make_music :: Word8 -> Integer -> Music Note -> ByteString
make_music dpb eot_delay = make_melody dpb eot_delay . events

make_melody :: Word8 -> Integer -> [PitchEvent] -> ByteString
make_melody dpb eot_delay notes =
  concat [ header Single 1 (Beats dpb), note_data, eot eot_delay ]
  where
    note_data = make_track $ concat $ map metaNote notes

-- Types

data Format = Single | Multi | Mixed

bs_format Single = 0x00
bs_format Multi  = 0x01
bs_format Mixed  = 0x02

data TimeDivision = Frames Word8 | Beats Word8

td_format (Frames f) = [0x80,f]
td_format (Beats  b) = [0x00,b]

type Track = ByteString  -- Rep for a track

-- If no tempo is defined, 120 beats per minute is assumed

{-
 Offset        Length  Type    Description             Value
 0x00          4       char[4] chunk ID                "MThd" (0x4D546864)
 0x04          4       dword   chunk size              6 (0x00000006)
 0x08          2       word    format type             0 - 2
 0x10          2       word    number of tracks        1 - 65,535
 0x12          2       word    time division           see following text
-}

header :: Format -> Word8 -> TimeDivision -> ByteString -- TODO: Use Integers, not Word8s
header format tracks time_division =
 concat $
   ["MThd"] -- Magic
   ++
   map pack [
     [0x00,0x00,0x00,0x06],     -- Magic
     [0x00, bs_format format],  -- Format
     [0x00, tracks],            -- Tracks
     td_format time_division -- Time division (Ticks per beat)
   ]


{-
 0x00          4       char[4] chunk ID        "MTrk" (0x4D54726B)
 0x04          4       dword   chunk size      see following text
 0x08          track event data (see following text)
-}

make_track t = concat [track_header t, t]

track_header :: Track -> ByteString
track_header track = concat $
 [ "MTrk" -- Magic
 , bin_num 4 $ fromIntegral (BS.length track)
 ]


instrument :: ByteString
instrument = pack [
  0x00, -- No Delay
  0xc0, -- Program Change, Channel 1
  0x18  -- Instrument 18
  ]

{-
 Delta Time            Event Type Value        MIDI Channel    Parameter 1     Parameter 2
 variable-length       4 bits                  4 bits          1 byte          1 byte
-}

event :: Integer -> ByteString -> ByteString
event delay item = bs_rep delay `append` item

metaNote (delay, On  note) = note_on  note delay
metaNote (delay, Off note) = note_off note delay

note_on :: Word8 -> Integer -> ByteString
note_on note delay = concat $ [
 bs_rep delay,
 pack [
   0x90, -- Note on, Channel 1
   note,
   0x60  -- Velocity
 ] ]

note_off :: Word8 -> Integer -> ByteString
note_off note delay = concat $ [
   bs_rep delay -- Two Ticks at Two beats / Second (120 beats / Min default)
 ]
 ++
 [ pack [
   0x80, -- Note on, Channel 1
   note,
   0x60  -- Velocity
 ] ]

{-
 End Of Track
 This meta event is used to signal the end of a track chunk and must always
 appear as the last event in every track chunk.

 Meta Event    Type            Length
 255 (0xFF)    47 (0x2F)       0
-}

eot :: Integer -> ByteString
eot when = concat [
   bs_rep when,
   pack [
     0xFF, -- Meta
     0x2F, -- EOT
     0x00  -- Data Length
   ]
 ]


-- Binary utils

{--- Acceptance Test
 Hex           Bin                             Hex     Bin
 00            00000000                        00      00000000
 C8            11001000                        8148    10000001 01001000
 100000        00010000 00000000 00000000      C08000  11000000 10000000 00000000
-}

bs_rep :: Integer -> ByteString
bs_rep = pack . reverse . fix . bin_rep 0x80

bin_rep :: Integer -> Integer -> [Integer]
bin_rep l n
 | d > 0     =  m : bin_rep l d
 | otherwise = [m]
 where (d,m) = divMod n l

fix :: [Integer] -> [Word8]
fix = map fromIntegral . zipWith (+) (0 : repeat 0x80)

bin_num :: Int -> Integer -> ByteString
bin_num len num = (pack . map fromIntegral . reverse) (components l n)
 where
   l  = fromIntegral len
   n  = fromIntegral num

components :: Integer -> Integer -> [Integer]
components c n
 | c <= 1 && n <= 0xFF = [n]
 | c == 1              = error "Number too long for byte-length"
 | otherwise           = m : components (c-1) d
   where (d,m) = n `divMod` (2^8)
