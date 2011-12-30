import Midi
import Data.List (tails)
import Data.List.Split
import qualified Data.ByteString as BS (writeFile)
import Text.Show.Pretty (ppDoc)


main = musicFile song

musicFile = BS.writeFile "Foo3.mid" . make_music 1 2

minor = concat . iterate (map (Higher 12)) $ [A,B,C,D,E,F,G]

fiths = iterate (drop 4) minor

triads = take 8 . map ( Parallel . take 3 . every 3 ) . tails

song = Sequence $ take 100 $ concatMap triads fiths


-- list utils

every n = map head . splitEvery n


-- Other song

writeMelString fn = BS.writeFile fn . make_music 1 2

melChar = Sequence . map nChar

nChar 'a' = A
nChar 'b' = B
nChar 'c' = C
nChar 'd' = D
nChar 'e' = E
nChar 'f' = F
nChar 'g' = G
nChar 'A' = Higher 12 A
nChar 'B' = Higher 12 B
nChar 'C' = Higher 12 C
nChar 'D' = Higher 12 D
nChar 'E' = Higher 12 E
nChar 'F' = Higher 12 F
nChar 'G' = Higher 12 G
nChar  x  = error $ show x ++ " is Not a Note"

song2 = melChar "gegACAgegg"

test = writeMelString "mel.mid" song2
