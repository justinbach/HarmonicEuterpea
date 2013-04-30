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
> import Data.Tuple
> import System.Random

> myPMap                       :: PlayerName -> Player Note1
> myPMap "CleanPlayer"         = cleanPlayer
> myPMap "RichPlayer"          = richPlayer
> myPMap "TritonePlayer"       = tritonePlayer
> myPMap "ReharmPlayer"        = reharmPlayer
> myPMap "ComboPlayer"         = comboPlayer

-------------------------------------------------------------------------------

This player cleanly interprets the underlying harmony without adding any color
tones. The harmonic textures it creates are rather lean, consisting solely of
the root, third, and seventh.

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
relying on the melody, diatonic context, and various other heuristics to
enforce the appropriate level of consonance and dissonence. The textures it
produces are substantially richer than those of the cleanPlayer.

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

This player plays creates harmonically rich textures in the manner of the
RichPlayer; however, it uses tritone substitutions on dominant chords to
transform the harmonic context.

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
It randomly picks a chord type and chord such that the melody note is the
chord's third, fifth, seventh, or ninth. It's most effective when used on
melodically simple songs with parallel melodic and harmonic motion, like
"Somewhere Over The Rainbow".

> reharmPlayer :: Player (Pitch, [NoteAttribute])
> reharmPlayer =  defPlayer
>               {pName        = "ReharmPlayer",
>                interpPhrase = reharmInterpPhrase}

> getReharmChord     :: Event -> SM (PhraseAttribute)
> getReharmChord mel =
>   do ct <- choose [Maj7, Min7]
>      cd <- choose [3, 5, 7, 9]
>      let melap   = ePitch mel
>          chap    = case (ct, cd) of
>                       (Min7, 3) -> melap - 3
>                       (_, 3) -> melap - 4
>                       (_, 5) -> melap - 7
>                       (Maj7, 7) -> melap - 11
>                       (_, 7) -> melap - 10
>                       (_, 9) -> melap - 2
>          pc      = fst $ pitch chap
>      return (Chord pc ct)


> reharmInterpPhrase :: PhraseFun a
> reharmInterpPhrase pm c [] m = perf pm c m
> reharmInterpPhrase pm c (pa:pas) m =
>   do  pfd@(pf, dur) <- fancyInterpPhrase pm c pas m
>       case pa of
>         ch@(Chord pc ct) ->
>             do reharmChord <- getReharmChord $ head pf
>                return (myChordHandler genRichChord reharmChord c pf, dur)
>         _ -> return pfd


-------------------------------------------------------------------------------

This player combines some of the above players by randomly choosing which
strategy to use when interpreting harmony. The randomness adds a nice element
of surprise, especially when reharmInterpPhrase is invoked.

> comboPlayer :: Player (Pitch, [NoteAttribute])
> comboPlayer =  defPlayer
>               {pName        = "ComboPlayer",
>                interpPhrase = comboInterpPhrase}

> comboInterpPhrase :: PhraseFun a
> comboInterpPhrase pm c [] m = perf pm c m
> comboInterpPhrase pm c pas m =
>   do interpPhrase <- choose [richInterpPhrase,
>                              tritoneInterpPhrase,
>                              reharmInterpPhrase]
>      interpPhrase pm c pas m


-------------------------------------------------------------------------------

The following helper functions provide a shorthand means of invoking a given player in the performance of a piece of music.

> playWithPlayer     :: String -> Music Pitch -> IO ()
> playWithPlayer p m = playA myPMap defCon $ toMusic1 $ Modify (Player p) $ m

> playClean :: Music Pitch -> IO ()
> playClean m = playWithPlayer "CleanPlayer" m

> playRich :: Music Pitch -> IO ()
> playRich m = playWithPlayer "RichPlayer" m

> playTritone :: Music Pitch -> IO ()
> playTritone m = playWithPlayer "TritonePlayer" m

> playReharm :: Music Pitch -> IO ()
> playReharm m = playWithPlayer "ReharmPlayer" m

> playCombo :: Music Pitch -> IO ()
> playCombo m = playWithPlayer "ComboPlayer" m

-------------------------------------------------------------------------------

These helper functions are used by the state monad for supplying random numbers.

> getRandom :: SM Int
> getRandom =  SM $ \s -> let (val, newGen) = next s in (newGen, val)

> choose :: [a] -> SM (a)
> choose l@(x:xs) =  do i <- getRandom
>                       let len = length l
>                       let index = i `mod` len
>                       return (l !! index)
> choose []       =  error "cannot choose from an empty list"
