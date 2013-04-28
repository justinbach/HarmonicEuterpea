Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

This file contains exports core Euterpea functionality for use in other modules, as well as the modifications I made to support harmonic context.

> module EuterpeaMods (module EuterpeaMods, module Euterpea) where
> import Euterpea hiding (PhraseAttribute(..), Control(..), Tempo(..), Music(..))

> data Music a  =
>        Prim (Primitive a)               -- primitive value
>     |  Music a :+: Music a              -- sequential composition
>     |  Music a :=: Music a              -- parallel composition
>     |  Modify Control (Music a)         -- modifier
>   deriving (Show, Eq, Ord)

> data PhraseAttribute  =  Dyn Dynamic
>                       |  Tmp Tempo
>                       |  Art Articulation
>                       |  Orn Ornament
>                       |  Chord PitchClass ChordType
>      deriving (Eq, Ord, Show)

> data Control =
>           Tempo       Rational           -- scale the tempo
>        |  Transpose   AbsPitch           -- transposition
>        |  Instrument  InstrumentName     -- instrument label
>        |  Phrase      [PhraseAttribute]  -- phrase attributes
>        |  Player      PlayerName         -- player label
>        |  KeySig      PitchClass Mode    -- key signature and mode
>   deriving (Show, Eq, Ord)

> data Tempo = Ritardando Rational | Accelerando Rational
>     deriving (Eq, Ord, Show)


> data ChordType =  Maj | Min | Dim | Aug | Maj7 | Min7 | Dom7 | HalfDim7 | Dim7 | MinMaj7 | AugMaj7
>     deriving (Eq, Ord, Show)
