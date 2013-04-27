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
> myPMap "CleanPlayer"         = cleanPlayer
> myPMap "RichPlayer"          = richPlayer
> myPMap "TritonePlayer"       = tritonePlayer

-------------------------------------------------------------------------------

This player cleanly interprets underlying harmony without adding any color tones.

> cleanPlayer :: Player (Pitch, [NoteAttribute])
> cleanPlayer =  defPlayer
>               {pName        = "CleanPlayer",
>                interpPhrase = cleanInterpPhrase}

> genCleanChord :: Context a -> Performance -> PitchClass -> ChordType ->Performance
> genCleanChord context pf pc ct =
>   let mel = head pf in
>   checkMelRange pf ((addRoot mel pc ct) ++ (add37 mel pc ct))

> myChordHandler :: (Context a -> Performance -> PitchClass -> ChordType -> Performance)
>                   -> PhraseAttribute -> Context a -> Performance -> Performance
> myChordHandler generator (Chord pc ct) context pf =
>     fixVols (fixDurs pf (generator context pf pc ct)) (head pf) ++ pf

> cleanInterpPhrase :: PhraseFun a
> cleanInterpPhrase pm c [] m = perf pm c m
> cleanInterpPhrase pm
>   c@Context {cTime = t, cPlayer = pl, cInst = i,
>              cDur = dt, cVol = v, cPch = pch, cKey = (pc, mode)}
>   (pa:pas) m =
>     let
>       pfd@(pf, dur) = fancyInterpPhrase pm c pas m
>     in
>       case pa of
>         ch@(Chord pc ct) -> (myChordHandler genCleanChord ch c pf, dur)
>         _ -> pfd


-------------------------------------------------------------------------------

This player adds color tones to the harmonic realization of the notated chords,
relying on the melody, diatonic context, and various other heuristics to enforce
the appropriate level of consonance and dissonence.

> richPlayer :: Player (Pitch, [NoteAttribute])
> richPlayer =  defPlayer
>               {pName        = "RichPlayer",
>                interpPhrase = richInterpPhrase}

> genRichChord :: Context a -> Performance -> PitchClass -> ChordType -> Performance
> genRichChord context pf pc ct =
>   let mel = head pf in
>   checkMelRange pf  ((addRoot mel pc ct) ++
>                     (add37 mel pc ct) ++
>                     dedup mel (addTensions context mel pc ct))

> richInterpPhrase :: PhraseFun a
> richInterpPhrase pm c [] m = perf pm c m
> richInterpPhrase pm
>   c@Context {cTime = t, cPlayer = pl, cInst = i,
>              cDur = dt, cVol = v, cPch = pch, cKey = (pc, mode)}
>   (pa:pas) m =
>     let
>       pfd@(pf, dur) = fancyInterpPhrase pm c pas m
>     in
>       case pa of
>         ch@(Chord pc ct) -> (myChordHandler genRichChord ch c pf, dur)
>         _ -> pfd


-------------------------------------------------------------------------------

This player plays creates harmonically rich textures in the manner of the RichPlayer,
but in addition, he always uses tritone substitutions for dominant chords.

> tritonePlayer :: Player (Pitch, [NoteAttribute])
> tritonePlayer =  defPlayer
>               {pName        = "TritonePlayer",
>                interpPhrase = tritoneInterpPhrase}

> tritoneInterpPhrase :: PhraseFun a
> tritoneInterpPhrase pm c [] m = perf pm c m
> tritoneInterpPhrase pm
>   c@Context {cTime = t, cPlayer = pl, cInst = i,
>              cDur = dt, cVol = v, cPch = pch, cKey = (pc, mode)}
>   (pa:pas) m =
>     let
>       pfd@(pf, dur) = fancyInterpPhrase pm c pas m
>       tritone pc = fst $ pitch $ (absPitch (pc, 0) + 6) `mod` 12
>     in
>       case pa of
>         ch@(Chord pc ct) ->
>           case ct of
>             -- TODO: fix richPlayer's tensions so chords sound good with tritone subs
>             Dom7 -> (myChordHandler genRichChord (Chord (tritone pc) ct) c pf, dur)
>             _ -> (myChordHandler genRichChord ch c pf, dur)
>         _ -> pfd




TODO: add more players:
- TritoneSubstPlayer - tritone substitutions on dominants (transforms pc and ct values)
- CrazySubstPlayer - constructs new harmonic context based on the melody note by deciding whether it should be the root, third, fifth, or seventh of the chord at random

