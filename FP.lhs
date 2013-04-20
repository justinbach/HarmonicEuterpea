Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module FP where
> import Euterpea
> import Data.List
> import Data.Ratio

This file serves to demonstrate some of the uses of the new Chord PhraseAttribute I've added to Music.hs. In particular, it creates several chord-tagged melodies, equivalent to a lead sheet, and constructs a few different players that perform them, interpreting the underlying harmony in different ways.


===============================================

This section handles the creation of a few lead sheets for testing.

> somewhere :: Music Pitch
> somewhere =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   ((Modify (Phrase [Chord Ef Maj])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord C Min7])) $ ef 6 hn) :+:
>   ((Modify (Phrase [Chord G Min7])) $ d 6 dqn :+: bf 5 sn :+: c 6 sn) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ d 6 qn) :+:
>   ((Modify (Phrase [Chord Ef Dom7])) $ ef 6 qn) :+:
>   ((Modify (Phrase [Chord Af Maj7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord A Dim7])) $ c 6 hn) :+:
>   ((Modify (Phrase [Chord Ef Maj])) $ bf 5 wn) :+:
>   ((Modify (Phrase [Chord Af Maj7])) $ c 5 hn) :+:
>   ((Modify (Phrase [Chord Af Min])) $ af 5 hn) :+:
>   ((Modify (Phrase [Chord G Min7])) $ g 5 dqn :+: ef 5 sn :+: f 5 sn) :+:
>   ((Modify (Phrase [Chord C Dom7])) $ g 5 qn :+: gs 5 qn) :+:
>   ((Modify (Phrase [Chord F Dom7])) $ f 5 dqn :+: d 5 sn :+: ef 5 sn) :+:
>   ((Modify (Phrase [Chord F Min7])) $ f 5 qn) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ g 5 qn) :+:
>   ((Modify (Phrase [Chord Ef Maj])) $ ef 5 wn)

> somewhere' :: Music Pitch
> somewhere' =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   ((Modify (Phrase [Chord Ef Maj7])) $ ef 6 hn)

Note that there's a cheat being used on the first note of Body and Soul, which is supposed to be a rest. In order to make the chordal texture kick in before the melody starts, I'm adding a note that would be masked by the voicing. This musical "hack" is used several times over the course of the piece.

> bodyAndSoul :: Music Pitch
> bodyAndSoul =
>   tempo dhn $
>   Modify (KeySig Df Major) $
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 3 qn :+: (tempo (3 % 2) (ef 5 en :+: f 5 en :+: ef 5 en))) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ f 5 en :+: ef 5 en :+: d 5 en :+: ef 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 5 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord D Dom7])) $ b 5 dqn :+: a 5 en) :+:

>   ((Modify (Phrase [Chord Df Maj7])) $ af 5 qn :+: (tempo (3 % 2) (af 5 en :+: bf 5 en :+: af 5 en))) :+:
>   ((Modify (Phrase [Chord Gf Dom7])) $ ef 6 dqn :+: c 6 en) :+:
>   ((Modify (Phrase [Chord F Min7])) $ ef 6 qn :+: df 6 qn) :+:
>   ((Modify (Phrase [Chord E Dim7])) $ c 6 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 4 qn :+: df 6 qn :+: (tempo (3 % 2) (bf 5 qn :+: gf 5 qn :+: bf 4 qn))) :+:
>   ((Modify (Phrase [Chord C HalfDim7])) $ f 5 hn) :+:
>   ((Modify (Phrase [Chord F Dom7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord Bf Min7])) $ bf 4 en :+: df 5 en) :+:
>   ((Modify (Phrase [Chord Ef Dom7])) $ ef 5 en :+: f 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ af 5 qn) :+:
>   ((Modify (Phrase [Chord Af Dom7])) $ (tempo (3 % 2) (af 5 en :+: bf 5 en :+: e 5 en))) :+:
>   ((Modify (Phrase [Chord Df Maj])) $ df 5 wn)

> whenIFallInLove :: Music Pitch
> whenIFallInLove =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   let oct = 5
>       a = ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord F Min7])) $ ef oct hn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ bf (oct - 1) hn) :+:
>           ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord F Dom7])) $ ef oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ f oct hn) :+:
>           ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord Af Dom7])) $ c (oct + 1) qn :+: bf oct qn) :+:
>           ((Modify (Phrase [Chord Df Dom7])) $ af oct hn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ g oct hn)
>   in
>     a

> altTest =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   let oct = 5 in

-->   ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:

>   ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn)



===============================================

This section handles the creation of different players for interpreting the above songs.

> myPMap                       :: PlayerName -> Player Note1
> myPMap "DiatonicPlayer"      = diatonicPlayer

> diatonicPlayer :: Player (Pitch, [NoteAttribute])
> diatonicPlayer =  defPlayer
>               {pName        = "DiatonicPlayer",
>                interpPhrase = diatonicInterpPhrase}

> diatonicInterpPhrase :: PhraseFun a
> diatonicInterpPhrase pm c [] m = perf pm c m
> diatonicInterpPhrase pm
>   c@Context {cTime = t, cPlayer = pl, cInst = i,
>              cDur = dt, cVol = v, cPch = pch, cKey = (pc, mode)}
>   (pa:pas) m =
>     let
>       pfd@(pf, dur) = fancyInterpPhrase pm c pas m
>     in
>       case pa of
>         ch@(Chord pc ct) -> (myPasChordHandler ch c pf, dur)
>         _ -> pfd

> myPasChordHandler (Chord pc ct) context pf =
>   let

This helper function is used in assessing what notes to include in a voicing, given the melody line.

>     areSamePC (e1, e2) =
>       let ((pc, _),(pc', _)) = (pitch (ePitch e1), pitch (ePitch e2)) in
>       pc' == pc

This helper function is used to remove from a voicing any pitch already in the melody.

>     dedup hd ns =
>           map (\(x, y) -> y) $ filter (not . areSamePC)
>             $ zip (replicate (length ns) hd) ns

These helper functions ensure that the notes selected for the voicing are no more than an octave above the a given threshold. The thresolds are as follows:

Root    : (A, 2)
3&7     : (D, 4)
Tensions: (G, 4)

>     aboveThreshold threshold e =
>       case compare (ePitch e) (absPitch threshold) of
>         LT -> aboveThreshold threshold e {ePitch = ePitch e + 12}
>         _  -> e {ePitch = ePitch e}

>     getRoot pc e = aboveThreshold (A, 2) e
>     get37 pc e = aboveThreshold (D, 4) e
>     getTensions pc e = aboveThreshold (G, 4) e


This helper function adds the root to the harmonic voicing.

>     addRoot hd pc ct =
>       [getRoot pc hd {ePitch = absPitch (pc, 0)}]


This helper function adds the core non-root chord tones to the voicing. Note that this only addresses 3rd and 7ths (where applicable); inclusion of the 5th should be determined on the basis of the melody, and so is classified as a tension.

>     add37 hd pc ct =
>       let ints = case ct of
>             Maj -> [4]
>             Min -> [3]
>             Dim -> [3]
>             Aug -> [4]
>             Maj7 -> [4, 11]
>             Min7 -> [3, 10]
>             Dom7 -> [4, 10]
>             HalfDim7 -> [3, 10]
>             Dim7 -> [3, 9]
>             MinMaj7 -> [3, 10]
>             AugMaj7 -> [4, 10]
>       in
>         map (get37 pc) $ map (\i -> hd {ePitch = absPitch(pc, 0) + i}) ints

The following functions add harmonic extensions to the voicing. In the case of DiatonicPlayer, any added extensions are diatonic to the key of the song.

>     majorInts = [0, 2, 4, 5, 7, 9, 11] -- major scale
>     majorTypes = [ -- the possible triads and seventh chords built on each scale degree
>       [Maj, Maj7],
>       [Min, Min7],
>       [Min, Min7],
>       [Maj, Maj7],
>       [Maj, Dom7],
>       [Min, Min7],
>       [Dim, HalfDim7]
>       ]
>
>     minorInts = [0, 2, 3, 5, 7, 9, 11] -- melodic
>     minorTypes = [
>       [Min, MinMaj7],
>       [Dim, HalfDim7],
>       [Maj, AugMaj7],
>       [Maj, Dom7],
>       [Maj, Dom7],
>       [Dim, HalfDim7],
>       [Dim, HalfDim7]
>       ]
>     isDiatonic context e = -- function to check whether a note is diatonic in the current context
>       let
>         (key, mode) = cKey context
>         ints = if mode == Major then majorInts else minorInts
>         cap = ePitch e `mod` 12
>         kap = absPitch (key, 0)
>         kInts = map (\i -> kap + i `mod` 12) ints
>       in
>         kap `elem` kInts

>     isDiatonicChord context pc ct = -- check whether a chord is diatonic to current context
>       let
>         (key, mode) = cKey context
>         (ints, types) = case mode of
>                           Major -> (majorInts, majorTypes)
>                           Minor -> (minorInts, minorTypes)
>         cap = absPitch (pc, 0)
>         kap = absPitch (key, 0)
>         kInts = map (\i -> kap + i `mod` 12) ints
>       in
>         case cap `elemIndex` kInts of
>           Just i  -> ct `elem` (types !! i)
>           Nothing -> False

>     isDissonent ap1 ap2 = -- utility for removing half-step conflicts with the melody
>       abs ((ap1 `mod` 12) - (ap2 `mod` 12)) == 1

>     addTensions hd pc ct =
>       let
>         diff = abs (ePitch hd `mod` 12) - (absPitch (pc, 0))
>         isAlt9th = diff == 1 || diff == 3 -- is the 9th altered in the melody?
>         isAlt5th = diff == 6 || diff == 8 -- is the 5th altered in the melody?
>         nat5th = [7]
>         flat5th = [6]
>         sharp5th = [8]
>         alt5th = flat5th ++ sharp5th
>         nat9th = [2]
>         flat9th = [1]
>         sharp9th = [3]
>         alt9th = flat9th ++ sharp9th
>         nat11th = [5]
>         nat13th = [9]
>         maybeAlt5th = if isAlt5th then alt5th else nat5th
>         maybeAlt9th = if isAlt9th then alt9th else nat9th
>         maybeAlt13th = if isAlt5th then [] else nat13th
>         remDissonence is =
>           let remove p = filter (not . p) in
>           remove (isDissonent (ePitch hd - absPitch(pc, 0))) is
>         ints = case ct of
>             Maj -> remDissonence $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>             Min -> remDissonence $ maybeAlt5th ++ maybeAlt9th
>             Dim -> flat5th
>             Aug -> sharp5th
>             Maj7 -> remDissonence $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>             Min7 -> remDissonence $ maybeAlt5th ++ nat9th -- ++ nat11th
>             Dom7 -> maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>             HalfDim7 -> flat5th
>             Dim7 -> flat5th
>             MinMaj7 -> maybeAlt5th
>             AugMaj7 -> sharp5th
>       in
>         case isDiatonicChord context pc ct of -- TODO: handle different cases?
>           _ -> filter (isDiatonic context) $ map (getTensions pc) $ map (\i -> hd {ePitch = absPitch(pc, 0) + i}) ints


This helper function removes any notes from the voicing that are pitched higher than the melody.

>     checkMelRange pf chord =
>         let
>           isLower e1 e2 res =
>             if res == False then False else ePitch e1 < ePitch e2
>           isLowerThanAll pf e = foldr (isLower e) True pf
>         in
>           filter (isLowerThanAll pf) chord

This helper function ties the various chord component builders together.

>     genChord pf pc ct =
>       let hd = head pf in
>       checkMelRange pf  ((addRoot hd pc ct) ++
>                         dedup hd ((add37 hd pc ct) ++
>                         (addTensions hd pc ct)))

These helper functions modify the notes to be as long as the phrase ("held down")

>     maxTime (s, e) t = max (((eTime e) - s) + (eDur e)) t
>     pfLength pf = foldr maxTime 0 (zip (replicate (length pf) (eTime (head pf))) pf)

>     fixDurs pf es =
>       let pfDur = pfLength pf in
>       map (\e -> e {eDur = pfDur}) es

This fixes the volume of the harmonic accompaniment to be less than that of the melody.

>     fixVols pf ref = map (\e -> e{eVol = 3 * ceiling ((fromIntegral (eVol ref)) / 4) }) pf

And finally, the body of the function adds the voicing to the melody line.

>   in
>     fixVols (fixDurs pf (genChord pf pc ct)) (head pf) ++ pf

Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "DiatonicPlayer") $ somewhere