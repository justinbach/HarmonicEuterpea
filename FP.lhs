Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module FP where
> import Euterpea
> import Data.List
> import Data.Ratio

This file serves to demonstrate some of the uses of the new Chord PhraseAttribute I've added to Music.hs. In particular, it uses a standard chord-tagged melody line, equivalent to a lead sheet, and constructs a few different players that perform the underlying harmony in different ways.


===============================================

This section handles the creation of a few lead sheets for testing.

> somewhere :: Music Pitch
> somewhere =
>   tempo dhn $
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

Note that there's a cheat on the first note of Body and Soul, which is supposed to be a rest. In order to make the chordal texture kick in before the melody starts, I'm adding a note that would be masked by the voicing. This musical "hack" is used several times over the course of the piece.

> bodyAndSoul :: Music Pitch
> bodyAndSoul =
>   tempo dhn $
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 3 qn :+: (tempo (3 % 2) (ef 5 en :+: f 5 en :+: ef 5 en))) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ f 5 en :+: ef 5 en :+: d 5 en :+: ef 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 5 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord D Dom7])) $ b 5 dqn :+: a 5 en) :+:

>   ((Modify (Phrase [Chord Df Maj7])) $ af 5 qn :+: (tempo (3 % 2) (af 5 en :+: bf 5 en :+: af 5 en))) :+:
>   ((Modify (Phrase [Chord Gf Dom7])) $ ef 6 dqn :+: c 6 en) :+:
>   ((Modify (Phrase [Chord F Min7])) $ ef 6 qn :+: df 6 qn) :+:
>   ((Modify (Phrase [Chord E Dim7])) $ c 6 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 4 qn :+: df 6 qn :+: (tempo (3 % 2) (bf 5 qn :+: gf 5 qn :+: bf 4 qn))) :+:
>   ((Modify (Phrase [Chord C Min7f5])) $ f 5 hn) :+:
>   ((Modify (Phrase [Chord F Dom7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord Bf Min7])) $ bf 3 en :+: df 5 en) :+:
>   ((Modify (Phrase [Chord Ef Dom7])) $ ef 5 en :+: f 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ af 5 qn) :+:
>   ((Modify (Phrase [Chord Af Dom7])) $ (tempo (3 % 2) (af 5 en :+: bf 5 en :+: e 5 en))) :+:
>   ((Modify (Phrase [Chord Df Maj])) $ df 5 wn)



===============================================

This section handles the creation of different players for interpreting the above songs.

> myPMap                       :: PlayerName -> Player Note1
> myPMap "ChordPlayer"          = chordPlayer

> chordPlayer :: Player (Pitch, [NoteAttribute])
> chordPlayer =  defPlayer
>               {pName        = "ChordPlayer",
>                interpPhrase = defInterpPhrase myPasChordHandler}

> myPasChordHandler (Chord pc ct) pf =
>   let

This helper function is used in assessing what notes to include in a voicing, given the melody line.

>     areSamePC (e1, e2) =
>       let ((pc, _),(pc', _)) = (pitch (ePitch e1), pitch (ePitch e2)) in
>       pc' == pc

This helper function is used to remove from a voicing any pitch already in the melody.

>     dedup hd ns =
>           map (\(x, y) -> y) $ filter (not . areSamePC)
>             $ zip (replicate (length ns) hd) ns

These helper functions ensure that the notes selected for the voicing fall within an acceptable range. The ranges are as follows:

Root    : (A, 2) - (C, 4)
357     : (D, 4) - (E, 5)
Tensions: (G, 4) - (A, 6)

>     getInRange lo hi initOct pc off e =
>       let
>         naiveAP = absPitch(pitch (absPitch (pc, initOct) + off))
>       in
>       case (compare naiveAP (absPitch lo), compare naiveAP (absPitch hi)) of
>         (LT, _) -> e {ePitch = absPitch (pc, initOct + 1)}
>         (_, GT) -> e {ePitch = absPitch (pc, initOct - 1)}
>         (_, _)  -> e {ePitch = naiveAP}

>     getRoot pc e = getInRange (A, 2) (C, 4) 3 pc 0 e
>     get357 pc off e  = getInRange (D, 4) (E, 5) 4 pc off e
>     getTensions pc off e = getInRange (G, 4) (A, 6) 5 pc off e


This helper function adds the root to the harmonic voicing.

>     addRoot hd pc ct =
>       [getRoot pc hd]

This helper function adds the core non-root chord tones to the voicing.

>     add357 hd pc ct =
>       case ct of
>         Maj ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             ns  = [iii, v]
>           in
>             dedup hd ns
>         Min ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 7 hd
>             ns  = [iii, v]
>           in
>             dedup hd ns
>         Dim ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 6 hd
>             ns  = [iii, v]
>           in
>             dedup hd ns
>         Aug ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             ns  = [iii, v]
>           in
>             dedup hd ns
>         Maj7 ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-1) hd
>             ns  = [iii, v, vii]
>           in
>             dedup hd ns
>         Min7 ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-2) hd
>             ns  = [iii, v, vii]
>           in
>             dedup hd ns
>         Dom7 ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-2) hd
>             ns  = [iii, v, vii]
>           in
>             dedup hd ns
>         Min7f5 ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 6 hd
>             vii = get357 pc (-2) hd
>             ns  = [iii, v, vii]
>           in
>             dedup hd ns
>         Dim7 ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 6 hd
>             vii = get357 pc 9 hd
>             ns  = [iii, v, vii]
>           in
>             dedup hd ns

This helper function adds harmonic extensions to the voicing, and uses nondeterminism to decide how to color the chord.

>     addTensions hd pc ct =
>       []

This helper function ties the various component builders together.

>     genChord pf pc ct =
>       let hd = head pf in
>       (addRoot hd pc ct) ++ (add357 hd pc ct) ++ (addTensions hd pc ct)

These helper functions modify the notes to be as long as the phrase ("held down")

>     maxTime (s, e) t = max (((eTime e) - s) + (eDur e)) t
>     pfLength pf = foldr maxTime 0 (zip (replicate (length pf) (eTime (head pf))) pf)

>     fixDurs pf es =
>       let pfDur = pfLength pf in
>       map (\e -> e {eDur = pfDur}) es

And finally, the body of the function adds the voicing to the melody line.

>   in
>     (fixDurs pf (genChord pf pc ct)) ++ pf

Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "ChordPlayer") $ somewhere