import Euterpea
import Euterpea.IO.MIDI.MidiIO (unsafeOutputID)

-- Attempt at transcribing ALL IS FULL OF LOVE by Bjork
-- from 34 Scores for Piano, Organ, Harpsichord and Celeste.
-- MIDI setup for Nord Drum 3p & Roland UM-ONE USB adapter.

-- The ID of the MIDI adapter changes when restarting, etc.
umOneID = Just (unsafeOutputID 2)

-- Nord Drum 3p setup code

nd3pMap :: ChannelMap
nd3pMap = zip pads [0 .. 5]
-- predefinedCP :: ChannelMap -> ChannelMapFun
nd3pParams = PlayParams False (predefinedCP nd3pMap) umOneID 1.0 perform1

playND3p n = playC nd3pParams n

pads = [AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano, 
        HonkyTonkPiano, RhodesPiano, ChorusedPiano]

bass_pads = take 2 pads
rhythm_pads = take 3 $ drop 2 pads
lead_pad = drop 5 pads

volumes = [127, 127, 96, 96, 96, 127, 127]

-- Utility functions

zipMeasure :: [Music Pitch] -> Music (Pitch, Volume)
-- Inputs a list of six pitches, assigns each to an instrument 
-- and combines them in parallel.
zipMeasure m = chord $ zipWith addVolume volumes $ zipWith instrument pads m

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
p2eighth (p, o) = dfnote en (p, o)

dfnote :: Dur -> Pitch -> Music Pitch
dfnote d p = note d (c2df p)

motif_line :: [Pitch] -> [Pitch] -> Music Pitch
motif_line a b = line $ map p2eighth (a ++ b)

-- Song constants

song_tempo = 80
t = song_tempo / 120

-- Building music

-- repeating motifs
-- rhythm motifs have top and bottom lines forming chords.
motif_a_top     = [(C, 5), (F, 4), (F, 4), (C, 5)]  
motif_a_bottom  = [(F, 4), (D, 4), (D, 4), (F, 4)]

motif_b_top     = [(F, 4), (F, 4), (C, 5), (D, 5)]
motif_b_bottom  = [(D, 4), (D, 4), (F, 4), (E, 4)]

motif_ab_top    = motif_line motif_a_top motif_b_top
motif_ab_bottom = motif_line motif_a_bottom motif_b_bottom

ab_5_variation  = [cut (7 * en) motif_ab_bottom :+: dfnote en (C, 4),
                   cut (7 * en) motif_ab_top]

motif_c_top     = [(G, 5), (A, 5), (A, 5), (G, 5)]
motif_c_bottom  = [(D, 5), (G, 4), (G, 4), (D, 5)]

motif_d_top     = [(A, 5), (A, 5), (G, 5), (A, 5)]
motif_d_bottom  = [(G, 4), (G, 4), (D, 5), (G, 4)]

motif_cd_top    = motif_line motif_c_top motif_d_top
motif_cd_bottom = motif_line motif_d_bottom motif_d_bottom

bass_d = [dfnote wn (D, 2), dfnote wn (A, 3)]
bass_g = [dfnote wn (G, 2), dfnote wn (D, 3)]


-- the song itself...

staff_1 = times 4 (zipMeasure [wnr, wnr, motif_ab_bottom, 
                               motif_ab_top, wnr, wnr]) :+: addVolume 0 enr

-- measure 5 uses a recurring variation on the ab motif
measure_5 = [dfnote wn (F, 2), wnr] ++ ab_5_variation ++ [wnr,
             line [dfnote qn (F, 3), dfnote qn (A, 4), 
             dfnote en (B, 3), dfnote qn (D, 3), dfnote en (F, 3)]]

-- measure 6 has variation on motif a and different rhythm
measure_6_rhythm :: [Pitch] -> Music Pitch
measure_6_rhythm l =
  let dur_6 = [qn, qn, en, dhn]
  in  line (zipWith dfnote dur_6 l)

measure_6 = bass_d ++ [measure_6_rhythm motif_a_bottom,
            measure_6_rhythm [(D, 5), (F, 4), (F, 4), (D, 5)], wnr,
            dfnote qn (F, 3)]

-- measure 7 introduces new rhythm motif
measure_7 = bass_g ++ [motif_cd_bottom, motif_cd_top, wnr,
            line [qnr, times 2 (dfnote qn (D, 3)), dfnote qn (F, 3)]]

measure_8 = bass_g ++ [motif_cd_bottom, motif_cd_top, wnr,
            line [dfnote en (F, 3), dfnote qn (E, 3), 
                  times 2 (dfnote en (E, 3)), dfnote dqn (D, 3)]]

staff_2 = line (map zipMeasure [measure_5, measure_6, measure_7, measure_8])

staffs = [staff_1,
          staff_2]

song = line staffs

main :: IO ()
main = 
    do
      playND3p (tempo t song)
