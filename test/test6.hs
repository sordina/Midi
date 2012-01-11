import Midi

main = write_music "test6.mid" 1 2 music

music :: Music Note
music = Sequence [ Parallel [C,E,G], Parallel [E,G,B], Parallel [G,C,E], C  ]
