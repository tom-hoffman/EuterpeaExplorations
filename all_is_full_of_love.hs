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

bass_pads = take 2 pads
rhythm_pads = drop 2 (take 2 pads)
lead_pads = drop 2 pads

nd3pMap :: ChannelMap
nd3pMap = zip pads [0 .. 5]
-- predefinedCP :: ChannelMap -> ChannelMapFun
nd3pParams = PlayParams False (predefinedCP nd3pMap) umOneID 1.0 perform1

playND3p n = playC nd3pParams n

-- Utility functions

zipMeasure :: [Music Pitch] -> Music Pitch
-- Inputs a list of six pitches, assigns each to an instrument 
-- and combines them in parallel.
zipMeasure m = chord (zipWith instrument pads m)

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

-- Song constants

song_tempo = 80
t = song_tempo / 120

-- Building music

-- repeating motifs
-- rhythm motifs have top and bottom lines forming chords.
motif_a_top    = [(C, 5), (F, 4), (F, 4), (C, 5)]  
motif_a_bottom = [(F, 4), (D, 4), (D, 4), (F, 4)]

motif_b_top    = [(F, 4), (F, 4), (C, 5), (D, 5)]
motif_b_bottom = [(D, 4), (D, 4), (F, 4), (E, 4)]

motif_top_line = line (map p2eighth (motif_a_top ++ motif_b_top))
motif_bottom_line = line (map p2eighth (motif_a_bottom ++ motif_b_bottom))

--measures

intro_measure = zipMeasure [wnr, wnr, motif_top_line, 
                            motif_bottom_line, wnr, wnr]

-- measure 5 ends the motif on a different note and the bass begins
measure_5 = zipMeasure [note wn (F, 2), wnr, 
                  cut (7 * en) motif_top_line,
                  cut (7 * en) motif_bottom_line :+: note en(C, 4), wnr, wnr]



---- measure 6 has variation on motif a and different rhythm
--measure_6_rhythm :: [Pitch] -> Music Pitch
--measure_6_rhythm l =
--  let dur_6 = [qn, qn, en, dhn]
--  in  line (zipWith note dur_6 l)
--top_6 = [(D, 5), (F, 4), (F, 4), (D, 5)]
--rhythm_6 = mergeChords rhythm_pads 
--             (map (measure_6_rhythm [top_6, (motif_a !! 1)]))
--measure_6 = rhythm_6
---- list of staffs

staffs = [times 4 intro_measure :+: enr,
          measure_5
         ]

song = line staffs

main :: IO ()
main = 
    do
      playND3p (tempo t song)
