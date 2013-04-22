Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module ChordFuns where
> import Euterpea
> import Data.List
> import Data.Ratio
> import Data.Maybe

> myPasChordHandler (Chord pc ct) context pf =
>   let

This helper function is used in assessing what notes to include in a voicing, given the melody line.

>     areSamePC (e1, e2) =
>       let ((pc, _),(pc', _)) = (pitch (ePitch e1), pitch (ePitch e2)) in
>       pc' == pc

This helper function is used to remove from a voicing any pitch already in the melody.

>     dedup mel ns =
>           map (\(x, y) -> y) $ filter (not . areSamePC)
>             $ zip (replicate (length ns) mel) ns

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

>     addRoot mel pc ct =
>       [getRoot pc mel {ePitch = absPitch (pc, 0)}]


This helper function adds the core non-root chord tones to the voicing. Note that this only addresses 3rd and 7ths (where applicable); inclusion of the 5th should be determined on the basis of the melody, and so is classified as a tension.

>     add37 mel pc ct =
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
>         map (get37 pc) $ map (\i -> mel {ePitch = absPitch(pc, 0) + i}) ints

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
>     minorInts = [0, 2, 3, 5, 7, 8, 11] -- harmonic
>     minorTypes = [
>       [Min, MinMaj7],
>       [Dim, HalfDim7],
>       [Maj, AugMaj7],
>       [Min, Min7],
>       [Maj, Dom7],
>       [Maj, Maj7],
>       [Dim, Dim7]
>       ]

>     getMode context = snd $ cKey context

>     getKey context = fst $ cKey context

>     getInts context =
>       if getMode context == Major then majorInts else minorInts

>     getTypes context =
>       if getMode context == Major then majorTypes else minorTypes

>     getScaleIndex context pc =
>       let
>         ints = getInts context
>         cap = absPitch (pc, 0)
>         kap = absPitch (getKey context, 0)
>         kInts = map (\i -> (kap + i) `mod` 12) ints
>       in
>         cap `elemIndex` kInts

>     isDiatonic context e = -- function to check whether a note is diatonic in the current context
>       case getScaleIndex context (fst $ pitch (ePitch e)) of
>       Just _ -> True
>       Nothing -> False

>     isDiatonicChord context pc ct = -- check whether a chord is diatonic to current context
>       let
>         (ints, types) = (getInts context, getTypes context)
>       in
>         case getScaleIndex context pc of
>           Just i  -> ct `elem` (types !! i)
>           Nothing -> False

>     isDissonent ap1 ap2 = -- utility for removing half-step conflicts with the melody
>       abs ((ap1 `mod` 12) - (ap2 `mod` 12)) `elem` [1] -- can be adjusted by adding more elements

>     remDissonence mel root is =
>       let remove p = filter (not . p) in
>       remove (isDissonent (ePitch root - absPitch(pc, 0))) $
>       remove (isDissonent (ePitch mel - absPitch(pc, 0))) is

>     addTensions mel pc ct =
>       let
>         root = head $ addRoot mel pc ct
>         addHeuristicTensions mel pc ct =
>           let
>             diff = abs (ePitch mel `mod` 12) - (absPitch (pc, 0))
>             isAlt9th = diff == 1 || diff == 3 -- is the 9th altered in the melody?
>             isAlt5th = diff == 6 || diff == 8 -- is the 5th altered in the melody?
>             nat5th = [7]
>             flat5th = [6]
>             sharp5th = [8]
>             alt5th = flat5th ++ sharp5th
>             nat9th = [2]
>             flat9th = [1]
>             sharp9th = [3]
>             alt9th = flat9th ++ sharp9th
>             nat11th = [5]
>             nat13th = [9]
>             maybeAlt5th = if isAlt5th then flat5th else nat5th -- TODO: use sharp5th? sometimes?
>             maybeAlt9th = if isAlt9th then flat9th else nat9th -- TODO: use sharp9th? sometimes?
>             maybeAlt13th = if isAlt5th then sharp5th else nat13th
>           in
>             case ct of
>               Maj -> remDissonence mel root $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>               Min -> remDissonence mel root $ maybeAlt5th ++ maybeAlt9th
>               Dim -> flat5th
>               Aug -> sharp5th
>               Maj7 -> remDissonence mel root $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>               Min7 -> remDissonence mel root $ maybeAlt5th ++ nat9th -- TODO: use nat11th?
>               Dom7 -> maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>               HalfDim7 -> flat5th
>               Dim7 -> flat5th
>               MinMaj7 -> maybeAlt5th
>               AugMaj7 -> sharp5th
>         addDiatonicTensions mel pc ct =
>           let
>             (key, mode) = cKey context
>             (ints, types) = case mode of
>                           Major -> (majorInts, majorTypes)
>                           Minor -> (minorInts, minorTypes)
>             cap = absPitch (pc, 0)
>             kap = absPitch (key, 0)
>             kInts = map (\i -> (kap + i) `mod` 12) ints

-->             scaleDegree = fromJust $ cap `elemIndex` kInts -- safe if cap is diatonic!

>             scaleDegree = fromJust $ getScaleIndex context pc
>             getOffset o = (ints !! ((scaleDegree + o) `mod` (length ints)))
>                            - (ints !! scaleDegree)
>             diatonic5th = [getOffset 4]
>             diatonic9th = [getOffset 1]
>             diatonic11th = [getOffset 3]
>             diatonic13th = [getOffset 5]
>           in
>             case ct of
>               Maj -> diatonic5th ++ diatonic9th ++ diatonic13th
>               Min -> remDissonence mel root $ diatonic5th ++ diatonic9th
>               Dim -> diatonic5th
>               Aug -> diatonic5th
>               Maj7 -> remDissonence mel root $ diatonic5th ++ diatonic9th ++ diatonic13th
>               Min7 -> remDissonence mel root $ diatonic5th ++ diatonic9th ++ diatonic11th
>               Dom7 -> diatonic5th ++ diatonic9th ++ diatonic13th
>               HalfDim7 -> diatonic5th
>               Dim7 -> diatonic5th
>               MinMaj7 ->diatonic5th
>               AugMaj7 -> diatonic5th
>         ints = case isDiatonicChord context pc ct of
>               True  -> addDiatonicTensions mel pc ct
>               False -> addHeuristicTensions mel pc ct
>       in
>       map (getTensions pc) $ map (\i -> mel {ePitch = absPitch(pc, 0) + i}) ints

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
>       let mel = head pf in
>       checkMelRange pf  ((addRoot mel pc ct) ++
>                         (add37 mel pc ct) ++
>                         dedup mel (addTensions mel pc ct))

These helper functions modify the notes to be as long as the phrase ("held down")

>     maxTime (s, e) t = max (((eTime e) - s) + (eDur e)) t
>     pfLength pf = foldr maxTime 0 (zip (replicate (length pf) (eTime (head pf))) pf)

>     fixDurs pf es =
>       let pfDur = pfLength pf in
>       map (\e -> e {eDur = pfDur}) es

This fixes the volume of the harmonic accompaniment to be less than that of the melody.

>     fixVols pf ref = map (\e -> e{eVol = 3 * ceiling ((fromIntegral (eVol ref)) / 4) }) pf

And finally, the body of the function adds the voicing to the melody line.


TODO: add support for substitutions by passing a modified chord type and pitch class to the helper functions.

>   in
>     fixVols (fixDurs pf (genChord pf pc ct)) (head pf) ++ pf

Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "DiatonicPlayer") $ somewhere
