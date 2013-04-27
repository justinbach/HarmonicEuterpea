Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

This file provides many useful functions to be invoked by the various players
when interpreting harmonic annotation in a given musical context.

> module ChordFuns where
> import Euterpea
> import Data.List
> import Data.Ratio
> import Data.Maybe

-------------------------------------------------------------------------------

This helper function is used in assessing what notes to include in a voicing, given the melody line.

> areSamePC          :: (Event, Event) -> Bool
> areSamePC (e1, e2) =
>   let ((pc, _),(pc', _)) = (pitch (ePitch e1), pitch (ePitch e2)) in
>   pc' == pc


-------------------------------------------------------------------------------

This helper function is used to remove from a voicing any pitch already in the melody.

> dedup        :: Event -> [Event] -> [Event]
> dedup mel ns =
>       map (\(x, y) -> y) $ filter (not . areSamePC)
>         $ zip (replicate (length ns) mel) ns


-------------------------------------------------------------------------------

These helper functions ensure that the notes selected for the voicing are no more than an octave above the a given threshold. The thresolds are as follows:

Root    : (A, 2)
3&7     : (D, 4)
Tensions: (G, 4)

> getAboveThreshold             :: Pitch -> Event -> Event
> getAboveThreshold threshold e =
>   case compare (ePitch e) (absPitch threshold) of
>     LT -> getAboveThreshold threshold e {ePitch = ePitch e + 12}
>     _  -> e {ePitch = ePitch e}

> getRoot      :: PitchClass -> Event -> Event
> getRoot pc e = getAboveThreshold (A, 2) e

> get37      :: PitchClass -> Event -> Event
> get37 pc e = getAboveThreshold (D, 4) e

> getTensions      :: PitchClass -> Event -> Event
> getTensions pc e = getAboveThreshold (G, 4) e


-------------------------------------------------------------------------------

This helper function adds the root to the harmonic voicing.

> addRoot           :: Event -> PitchClass -> ChordType -> [Event]
> addRoot mel pc ct = [getRoot pc mel {ePitch = absPitch (pc, 0)}]


-------------------------------------------------------------------------------

This helper function adds the core non-root chord tones to the voicing. Note that this only addresses 3rd and 7ths (where applicable); inclusion of the 5th should be determined on the basis of the melody, and so is classified as a tension.

> add37           :: Event -> PitchClass -> ChordType -> [Event]
> add37 mel pc ct =
>   let ints = case ct of
>         Maj -> [4]
>         Min -> [3]
>         Dim -> [3]
>         Aug -> [4]
>         Maj7 -> [4, 11]
>         Min7 -> [3, 10]
>         Dom7 -> [4, 10]
>         HalfDim7 -> [3, 10]
>         Dim7 -> [3, 9]
>         MinMaj7 -> [3, 10]
>         AugMaj7 -> [4, 10]
>   in
>     map (get37 pc) $ map (\i -> mel {ePitch = absPitch(pc, 0) + i}) ints


-------------------------------------------------------------------------------

The following functions provide tonality-related utilities. They are
primarily used by the addTensions function.

> majorInts :: [AbsPitch]
> majorInts = [0, 2, 4, 5, 7, 9, 11] -- major scale

> majorTypes :: [[ChordType]]
> majorTypes = [ -- the possible triads and seventh chords built on each scale degree
>   [Maj, Maj7],
>   [Min, Min7],
>   [Min, Min7],
>   [Maj, Maj7],
>   [Maj, Dom7],
>   [Min, Min7],
>   [Dim, HalfDim7]
>   ]

> minorInts :: [AbsPitch]
> minorInts = [0, 2, 3, 5, 7, 8, 11] -- harmonic

> minorTypes :: [[ChordType]]
> minorTypes = [
>   [Min, MinMaj7],
>   [Dim, HalfDim7],
>   [Maj, AugMaj7],
>   [Min, Min7],
>   [Maj, Dom7],
>   [Maj, Maj7],
>   [Dim, Dim7]
>   ]

> getMode         :: Context a -> Mode
> getMode context = snd $ cKey context

> getKey         :: Context a -> PitchClass
> getKey context = fst $ cKey context

> getInts         :: Context a -> [AbsPitch]
> getInts context =
>   if getMode context == Major then majorInts else minorInts

> getTypes         :: Context a -> [[ChordType]]
> getTypes context =
>   if getMode context == Major then majorTypes else minorTypes

> getScaleIndex            :: Context a -> PitchClass -> Maybe Int
> getScaleIndex context pc =
>   let
>     ints = getInts context
>     cap = absPitch (pc, 0)
>     kap = absPitch (getKey context, 0)
>     kInts = map (\i -> (kap + i) `mod` 12) ints
>   in
>     cap `elemIndex` kInts

> isDiatonic           :: Context a -> Event -> Bool
> isDiatonic context e = -- function to check whether a note is diatonic in the current context
>   case getScaleIndex context (fst $ pitch (ePitch e)) of
>   Just _ -> True
>   Nothing -> False

> isDiatonicChord      :: Context a -> PitchClass -> ChordType -> Bool
> isDiatonicChord context pc ct = -- check whether a chord is diatonic to current context
>   let
>     (ints, types) = (getInts context, getTypes context)
>   in
>     case getScaleIndex context pc of
>       Just i  -> ct `elem` (types !! i)
>       Nothing -> False


> isDissonent         :: AbsPitch -> AbsPitch -> Bool
> isDissonent ap1 ap2 = -- utility for removing half-step conflicts with the melody
>   abs ((ap1 `mod` 12) - (ap2 `mod` 12)) `elem` [1] -- can be adjusted by adding more elements

> remDissonence         :: PitchClass -> Event -> [AbsPitch] -> [AbsPitch]
> remDissonence pc e is =
>   let remove p = filter (not . p) in
>       remove (isDissonent (ePitch e - absPitch(pc, 0))) is

> remDissonenceMelRoot :: PitchClass -> Event -> Event -> [AbsPitch] -> [AbsPitch]
> remDissonenceMelRoot pc mel root is =
>   remDissonence pc root $ remDissonence pc mel is


-------------------------------------------------------------------------------

The following function adds color tones, or "tensions", to the harmonic
realization of a chord. There are two general situations that it handles.
In the first, the chord is built on a diatonic scale degree of the current
key, and tensions are added from the diatonic scale of the key. In the second,
heuristic measures are invoked (e.g. melodic) to determine which sorts of
harmonic extensions might be appropriate. Both implementations, but especially
the latter, are much more "ad hoc" than I would like.

> addTensions :: Context a -> Event -> PitchClass -> ChordType -> [Event]
> addTensions context mel pc ct =
>   let
>     root = head $ addRoot mel pc ct
>     melpc = fst $ pitch $ ePitch mel
>     remDissonence' = remDissonenceMelRoot pc mel root
>     addDiatonicTensions mel pc ct =
>       let
>         scaleDegree = fromJust $ getScaleIndex context pc
>         ints        = getInts context
>         getOffset o = (ints !! ((scaleDegree + o) `mod` (length ints)))
>                        - (ints !! scaleDegree)
>         diatonic5th = [getOffset 4]
>         diatonic9th = [getOffset 1]
>         diatonic11th = [getOffset 3]
>         -- avoid clashes between natural and sharp 5
>         diatonic13th = case isDissonent (getOffset 5) (head diatonic5th) of
>                         True -> []
>                         False -> [getOffset 5]
>       in
>         case ct of
>           Maj -> diatonic5th ++ diatonic9th ++ diatonic13th
>           Min -> remDissonence' $ diatonic5th ++ diatonic9th
>           Dim -> diatonic5th
>           Aug -> diatonic5th
>           Maj7 -> remDissonence' $ diatonic5th ++ diatonic9th ++ diatonic13th
>           Min7 -> remDissonence' $ diatonic5th ++ diatonic9th ++ diatonic11th
>           Dom7 -> diatonic5th ++ diatonic9th ++ diatonic13th
>           HalfDim7 -> diatonic5th
>           Dim7 -> diatonic5th
>           MinMaj7 ->diatonic5th
>           AugMaj7 -> diatonic5th
>     addHeuristicTensions mel pc ct =
>       let
>         diff = abs (ePitch mel `mod` 12) - (absPitch (pc, 0))
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
>         maybeAlt5th = if isAlt5th then flat5th else nat5th -- TODO: use sharp5th sometimes?
>         maybeAlt9th = if isAlt9th then flat9th else nat9th -- TODO: use sharp9th sometimes?
>         maybeAlt13th = if isAlt5th then sharp5th else nat13th
>       in
>         case ct of
>           Maj -> remDissonence' $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>           Min -> remDissonence' $ maybeAlt5th ++ maybeAlt9th
>           Dim -> flat5th
>           Aug -> sharp5th
>           Maj7 -> remDissonence' $ maybeAlt5th ++ maybeAlt9th ++ maybeAlt13th
>           Min7 -> remDissonence' $ maybeAlt5th ++ nat9th -- TODO: use nat11th?
>           Dom7 -> maybeAlt5th ++ maybeAlt9th  ++ maybeAlt13th
>           HalfDim7 -> flat5th
>           Dim7 -> flat5th
>           MinMaj7 -> maybeAlt5th
>           AugMaj7 -> sharp5th
>     ints = case (getScaleIndex context pc, getScaleIndex context melpc) of
>           (Just _, Just _) -> addDiatonicTensions mel pc ct
>           _ -> addHeuristicTensions mel pc ct
>   in
>   map (getTensions pc) $ map (\i -> mel {ePitch = absPitch(pc, 0) + i}) ints


-------------------------------------------------------------------------------

This helper function removes any notes from the voicing that are pitched
higher than the melody.

> checkMelRange          :: [Event] -> [Event] -> [Event]
> checkMelRange pf chord =
>     let
>       isLower e1 e2 res =
>         if res == False then False else ePitch e1 < ePitch e2
>       isLowerThanAll pf e = foldr (isLower e) True pf
>     in
>       filter (isLowerThanAll pf) chord


-------------------------------------------------------------------------------

These helper functions extend the voicing to be as long as the phrase.

> maxTime          :: (Ratio Integer, Event) -> Ratio Integer -> Ratio Integer
> maxTime (s, e) t = max (((eTime e) - s) + (eDur e)) t

> pfLength    :: [Event] -> Ratio Integer
> pfLength pf = foldr maxTime 0 (zip (replicate (length pf) (eTime (head pf))) pf)

> fixDurs       :: [Event] -> [Event] -> [Event]
> fixDurs pf es =
>   let pfDur = pfLength pf in
>   map (\e -> e {eDur = pfDur}) es


-------------------------------------------------------------------------------

This adjusts the volume of the harmonic accompaniment to be less than that
of the melody.

> fixVols        :: [Event] -> Event -> [Event]
> fixVols pf ref = map (\e -> e{eVol = 3 * ceiling ((fromIntegral (eVol ref)) / 4) }) pf
