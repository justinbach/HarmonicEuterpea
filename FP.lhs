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
>   tempo hn $
>   ((Modify (Phrase [Chord Ef Maj7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord C Min7])) $ ef 6 hn)


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

This helper function adds the root to the harmonic voicing.

>     addRoot hd pc ct =
>       [hd {ePitch = absPitch (pc, 3)}] -- TODO: fix maxTime implementation

This helper function adds the core non-root chord tones to the voicing.

>     add357 hd pc ct =
>       []

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