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
>   (Modify (Phrase [Chord Ef Maj7])) $ ef 4 hn :+: ef 5 hn


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
>     maxTime e t = max ((eTime e) + (eDur e)) t
>     pfLength pf = foldr maxTime 0 pf
>     addRoot pf =
>       let hd = head pf in
>       (hd {ePitch = ePitch hd - 20, eDur = (pfLength pf)}):pf
>   in
>     addRoot pf

Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "ChordPlayer") $ somewhere