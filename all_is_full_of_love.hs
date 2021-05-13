import Euterpea
import Euterpea.IO.MIDI.MidiIO (unsafeOutputID)

-- Attempt at transcribing ALL IS FULL OF LOVE by Bjork
-- from 34 Scores for Piano, Organ, Harpsichord and Celeste.
-- MIDI setup for Nord Drum 3p & Roland UM-ONE USB adapter.

-- The ID of the MIDI adapter changes when restarting, etc.
umOneID = Just (unsafeOutputID 2)

-- Nord Drum 3p setup code

pads = [AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano, 
        HonkyTonkPiano, RhodesPiano, ChorusedPiano]

top_bass = pads !! 0
bottom_bass = pads !! 1
top_rhythm = pads !! 2
bottom_rhythm = pads !! 3

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
mergeRhythmChords l = instrument top_rhythm (l !! 0) :=: 
                      instrument bottom_rhythm (l !! 1)

-- Song constants

song_tempo = 80
t = song_tempo / 120

-- Building music

-- repeating motifs
-- rhythm motifs have top and bottom lines forming chords.
motif_a = [[(C, 5), (F, 4), (F, 4), (C, 5)],  
           [(F, 4), (D, 4), (D, 4), (F, 4)]]

motif_b = [[(F, 4), (F, 4), (C, 5), (D, 5)],
           [(D, 4), (D, 4), (F, 4), (E, 4)]]

motif_a_line = map make8thNotes motif_a
motif_b_line = map make8thNotes motif_b
-- repeating measures

intro_measure = mergeRhythmChords motif_a_line :+: 
                mergeRhythmChords motif_b_line

-- singleton measures
rhythm_5b = (cut (7 * en) intro_measure :+: 
             instrument bottom_rhythm (note en (C, 4))) :=:
             instrument top_bass (note wn (F, 2))

--rhythm_6 = make8thNotes 
-- use zipWith to merge a list of durs with a list of pitches.

-- list of staffs

staffs = [times 4 intro_measure :+: enr,                                     --1
          rhythm_5b]

song = line staffs

main :: IO ()
main = 
  do
    playND3p (tempo t song)
