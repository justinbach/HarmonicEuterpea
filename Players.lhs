Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module Players where
> import Euterpea
> import ChordFuns
> import Data.List
> import Data.Ratio
> import Data.Maybe

The code in this file constructs a few players, which use the functions for creating voicings in interesting and different ways.

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

TODO: add more players:
- CleanPlayer - just plays 3rd and seventh, and occasionally fifth (maybe)
- RichPlayer - future version of diatonicPlayer
- TritoneSubstPlayer - tritone substitutions on dominants (transforms pc and ct values)
- CrazySubstPlayer - constructs new harmonic context based on the melody note by deciding whether it should be the root, third, fifth, or seventh of the chord at random

