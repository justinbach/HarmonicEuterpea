Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

The code in this file constructs a few players, which use the functions for
creating voicings in interesting and different ways.

> module Players where
> import EuterpeaMods
> import ChordFuns
> import Data.List
> import Data.Ratio
> import Data.Maybe
> import System.Random
> import System.IO.Unsafe -- just for random numbers!

> myPMap                       :: PlayerName -> Player Note1
> myPMap "CleanPlayer"         = cleanPlayer
> myPMap "RichPlayer"          = richPlayer
> myPMap "TritonePlayer"       = tritonePlayer
> myPMap "ReharmPlayer"        = reharmPlayer
> myPMap "ComboPlayer"         = comboPlayer

-------------------------------------------------------------------------------

This player cleanly interprets underlying harmony without adding any color tones.

> cleanPlayer :: Player (Pitch, [NoteAttribute])
> cleanPlayer =  defPlayer
>               {pName        = "CleanPlayer",
>                interpPhrase = cleanInterpPhrase}

> genCleanChord :: Context a -> Performance -> PitchClass -> ChordType -> Performance
> genCleanChord context pf pc ct =
>   let mel = head pf in
>   checkMelRange pf ((addRoot mel pc ct) ++ (add37 mel pc ct))

> myChordHandler :: (Context a -> Performance -> PitchClass -> ChordType -> Performance)
>                   -> PhraseAttribute -> Context a -> Performance -> Performance
> myChordHandler generator (Chord pc ct) context pf =
>     fixVols (fixDurs pf (generator context pf pc ct)) (head pf) ++ pf

> cleanInterpPhrase :: PhraseFun a
> cleanInterpPhrase pm c [] m = perf pm c m
> cleanInterpPhrase pm c (pa:pas) m =
>   do pfd@(pf, dur) <- fancyInterpPhrase pm c pas m
>      case pa of
>        ch@(Chord pc ct) -> return (myChordHandler genCleanChord ch c pf, dur)
>        _ -> return pfd


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
> richInterpPhrase pm c (pa:pas) m =
>   do pfd@(pf, dur) <- fancyInterpPhrase pm c pas m
>      case pa of
>        ch@(Chord pc ct) -> return (myChordHandler genRichChord ch c pf, dur)
>        _ -> return pfd


-------------------------------------------------------------------------------

This player plays creates harmonically rich textures in the manner of the RichPlayer,
but in addition, it always uses tritone substitutions for dominant chords.

> tritonePlayer :: Player (Pitch, [NoteAttribute])
> tritonePlayer =  defPlayer
>               {pName        = "TritonePlayer",
>                interpPhrase = tritoneInterpPhrase}

> tritoneInterpPhrase :: PhraseFun a
> tritoneInterpPhrase pm c [] m = perf pm c m
> tritoneInterpPhrase pm c (pa:pas) m =
>   let
>     tritone pc = fst $ pitch $ (absPitch (pc, 0) + 6) `mod` 12
>   in
>   do pfd@(pf, dur) <- fancyInterpPhrase pm c pas m
>      case pa of
>        ch@(Chord pc ct) ->
>          case ct of
>            -- TODO: fix richPlayer's tensions so chords sound good with tritone subs
>            Dom7 -> return (myChordHandler genCleanChord (Chord (tritone pc) ct) c pf, dur)
>            _ -> return (myChordHandler genRichChord ch c pf, dur)
>        _ -> return pfd

-------------------------------------------------------------------------------

This player creates radical reharmonizations solely on the basis of the melody.
It randomly picks a chord type and chord such that the melody note is the chord's
third, fifth, seventh, or ninth.

> reharmPlayer :: Player (Pitch, [NoteAttribute])
> reharmPlayer =  defPlayer
>               {pName        = "ReharmPlayer",
>                interpPhrase = reharmInterpPhrase}

> getReharmChord     :: Event -> PhraseAttribute
> getReharmChord mel =
>   let chordTypes = [Maj7, Min7]
>       chordDegs  = [3, 5, 7, 9]
>       ctIndex    = (unsafePerformIO $ randomIO) `mod` (length chordTypes)
>       cdIndex    = (unsafePerformIO $ randomIO) `mod` (length chordDegs)
>       ct         = chordTypes !! ctIndex
>       cd         = chordDegs !! cdIndex
>       melap      = ePitch mel
>       chap       = case (ct, cd) of
>                       (Min7, 3) -> melap - 3
>                       (_, 3) -> melap - 4
>                       (_, 5) -> melap - 7
>                       (Maj7, 7) -> melap - 11
>                       (_, 7) -> melap - 10
>                       (_, 9) -> melap - 2
>       pc         = fst $ pitch chap
>   in
>   Chord pc ct


> reharmInterpPhrase :: PhraseFun a
> reharmInterpPhrase pm c [] m = perf pm c m
> reharmInterpPhrase pm c (pa:pas) m =
>   do  pfd@(pf, dur) <- fancyInterpPhrase pm c pas m
>       case pa of
>         ch@(Chord pc ct) ->
>             return (myChordHandler genRichChord (getReharmChord $ head pf) c pf, dur)
>         _ -> return pfd

-------------------------------------------------------------------------------

This player combines some of the above players by randomly choosing which strategy
to use when interpreting harmony.

> comboPlayer :: Player (Pitch, [NoteAttribute])
> comboPlayer =  defPlayer
>               {pName        = "ComboPlayer",
>                interpPhrase = comboInterpPhrase}

> comboInterpPhrase :: PhraseFun a
> comboInterpPhrase pm c [] m = perf pm c m
> comboInterpPhrase pm c pas m =
>   let interpPhrases = [richInterpPhrase,
>                        tritoneInterpPhrase,
>                        reharmInterpPhrase]
>       interpIndex   = (unsafePerformIO $ randomIO) `mod` (length interpPhrases)
>       interpPhrase  = interpPhrases !! interpIndex
>   in  interpPhrase pm c pas m