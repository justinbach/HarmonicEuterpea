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
>   ((Modify (Phrase [Chord Ef Maj])) $ bf 5 wn)




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

These two helper functions are used to calculate the duration of the entire phrase.

>     maxTime e t = max ((eTime e) + (eDur e)) t
>     pfLength pf = foldr maxTime 0 pf

This helper function is used in assessing what notes to include in a voicing, given the melody line.

>     areSamePC (e1, e2) =
>       let ((pc, _),(pc', _)) = (pitch (ePitch e1), pitch (ePitch e2)) in
>       pc' == pc

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


This helper function adds the root to the harmonic voicing.

>     addRoot hd pc ct =
>       [getRoot pc hd] -- TODO: fix maxTime implementation

This helper function adds the core non-root chord tones to the voicing.

>     add357 hd pc ct =
>       let
>         dedup ns = -- don't duplicate notes in the melody!
>           map (\(x, y) -> y) $ filter (not . areSamePC)
>             $ zip (replicate (length ns) hd) ns
>       in
>       case ct of
>         Maj ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             ns  = [iii, v]
>           in
>             dedup ns
>         Min ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 7 hd
>             ns  = [iii, v]
>           in
>             dedup ns
>         Maj7 ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-1) hd
>             ns  = [iii, v, vii]
>           in
>             dedup ns
>         Min7 ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-2) hd
>             ns  = [iii, v, vii]
>           in
>             dedup ns
>         Dom7 ->
>           let
>             iii = get357 pc 4 hd
>             v   = get357 pc 7 hd
>             vii = get357 pc (-2) hd
>             ns  = [iii, v, vii]
>           in
>             dedup ns
>         Dim7 ->
>           let
>             iii = get357 pc 3 hd
>             v   = get357 pc 6 hd
>             vii = get357 pc 9 hd
>             ns  = [iii, v, vii]
>           in
>             dedup ns
>         _    -> []

This helper function adds harmonic extensions and color tones to the voicing.

>     addTensions hd pc ct =
>       []

This helper function ties the various component builders together.

>     genChord pf pc ct =
>       let hd = head pf in
>       (addRoot hd pc ct) ++ (add357 hd pc ct) ++ (addTensions hd pc ct)
>   in

And finally, the body of the function adds the voicing to the melody line.

>     (genChord pf pc ct) ++ pf

Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "ChordPlayer") $ somewhere