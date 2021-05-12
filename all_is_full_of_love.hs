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

makeRhythmChords :: Music Pitch -> Music Pitch -> Music Pitch
-- Assign to first two pads and merge.
makeRhythmChords t b = instrument top_rhythm t :=: instrument bottom_rhythm b


-- Building music

-- repeating motifs
-- rhythm motifs have top and bottom lines forming chords.
motif_a = [[(C, 5), (F, 4), (F, 4), (C, 5)],  
           [(F, 4), (D, 4), (D, 4), (F, 4)]]

motif_b = [[(F, 4), (F, 4), (C, 5), (D, 5)],
           [(D, 4), (D, 4), (F, 4), (E, 4)]]

-- repeating measures

intro_measure = makeRhythmChords motif_a_top motif_a_bottom :+:
                  makeRhythmChrods motif_b_top motif_b_bottom


measure_5b_top = init motif_b_top
line_5b_top = motif2eline (pads !! 0) motif_a_top
measure_5b_bottom = init motif_b_top ++ [(C, 4)]
line_5b_bottom = motif2eline (pads !! 0) motif_a_bottom
line_5_bass = instrument (pads !! 2) (note wn (F, 2))

motif_a = line_a_top :=: line_a_bottom
motif_b = line_b_top :=: line_b_bottom
intro_motif = motif_a :+: motif_b 
measure_5b = line_5b_top :=: line_5b_bottom :=: line_5_bass

song = times 4 intro_motif :+: enr :+: measure_5b

main :: IO ()
main = 
  do
    playND3p (tempo t song)
