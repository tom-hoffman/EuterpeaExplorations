import Euterpea
import Euterpea.IO.MIDI.MidiIO (unsafeOutputID)

-- Attempt at transcribing ALL IS FULL OF LOVE by Bjork
-- from 34 Scores for Piano, Organ, Harpsichord and Celeste.
-- MIDI setup for Nord Drum 3p & Roland UM-ONE USB adapter.

-- This changes periodically.
umOneID = Just (unsafeOutputID 2)

t = 80 / 120

-- Nord Drum 3p setup code

pads = [AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano, 
        HonkyTonkPiano, RhodesPiano, ChorusedPiano]

bass = pads !! 0
top_rhythm = pads !! 1
bottom_rhythm = pads !! 2

nd3pMap :: ChannelMap
nd3pMap = zip pads [0 .. 5]
-- predefinedCP :: ChannelMap -> ChannelMapFun
nd3pParams = PlayParams False (predefinedCP nd3pMap) umOneID 1.0 perform1

playND3p n = playC nd3pParams n

-- Utility functions

c2df :: Pitch -> Pitch
-- Fixes pitches entered from D flat sheet music without flats.
-- That is, as if they were in C.
c2df (G, o) = (Gf, o)
c2df (A, o) = (Af, o)
c2df (B, o) = (Bf, o)
c2df (D, o) = (Df, o)
c2df (E, o) = (Ef, o)
c2df (p, o) = (p, o)

p2eighth :: Pitch -> Music Pitch
-- Sets a pitch to an eighth note.
p2eighth (p, o) = note en (p, o)

make8thNotes :: [Pitch] -> Music Pitch
make8thNotes l = line (map p2eighth (map c2df l))

mergeRhythmChords :: [Music Pitch] -> Music Pitch
-- Assign to first two pads and merge.
mergeRhythmChords l = instrument top_rhythm (head l) :=: 
                        instrument bottom_rhythm (last b)


-- Building music

-- repeating motifs
-- rhythm motifs have top and bottom lines forming chords.
motif_a = [[(C, 5), (F, 4), (F, 4), (C, 5)],  
           [(F, 4), (D, 4), (D, 4), (F, 4)]]

motif_b = [[(F, 4), (F, 4), (C, 5), (D, 5)],
           [(D, 4), (D, 4), (F, 4), (E, 4)]]

-- repeating measures

intro_measures = mergeRhythmChords motif_a :+: mergeRhythmChrods motif_b

-- singleton measures
measure_5b_top = init (head motif_b) -- drop the last note of the top line
measure_5b_bottom = init (last motif_b) ++ [(C, 4)]

line_5_bass = instrument (pads !! 2) (note wn (F, 2))

measure_5b = line_5b_top :=: line_5b_bottom :=: line_5_bass

song = times 4 intro_motif :+: enr :+: 
         measure_5b

main :: IO ()
main = 
  do
    playND3p (tempo t song)
