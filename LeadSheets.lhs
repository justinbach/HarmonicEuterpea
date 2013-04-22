Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module LeadSheets where
> import Euterpea
> import Data.Ratio

This file serves to demonstrate some of the uses of the new Chord PhraseAttribute I've added to Music.hs. In particular, it creates several chord-tagged melodies, equivalent to a lead sheet, and constructs a few different players that perform them, interpreting the underlying harmony in different ways.



This file contains a few "lead sheets" to be performed by various players.

> somewhere :: Music Pitch
> somewhere =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   ((Modify (Phrase [Chord Ef Maj])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord C Min7])) $ ef 6 hn) :+:
>   ((Modify (Phrase [Chord G Min7])) $ d 6 dqn :+: bf 5 sn :+: c 6 sn) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ d 6 qn) :+:
>   ((Modify (Phrase [Chord Ef Dom7])) $ ef 6 qn) :+:
>   ((Modify (Phrase [Chord Af Maj7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord A Dim7])) $ c 6 hn) :+:
>   ((Modify (Phrase [Chord Ef Maj])) $ bf 5 wn) :+:
>   ((Modify (Phrase [Chord Af Maj7])) $ c 5 hn) :+:
>   ((Modify (Phrase [Chord Af Min])) $ af 5 hn) :+:
>   ((Modify (Phrase [Chord G Min7])) $ g 5 dqn :+: ef 5 sn :+: f 5 sn) :+:
>   ((Modify (Phrase [Chord C Dom7])) $ g 5 qn :+: gs 5 qn) :+:
>   ((Modify (Phrase [Chord F Dom7])) $ f 5 dqn :+: d 5 sn :+: ef 5 sn) :+:
>   ((Modify (Phrase [Chord F Min7])) $ f 5 qn) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ g 5 qn) :+:
>   ((Modify (Phrase [Chord Ef Maj])) $ ef 5 wn)

> somewhere' :: Music Pitch
> somewhere' =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   ((Modify (Phrase [Chord C Min7])) $ ef 6 hn)

Note that there's a cheat being used on the first note of Body and Soul, which is supposed to be a rest. In order to make the chordal texture kick in before the melody starts, I'm adding a note out of the melodic range. This musical "hack" is used several times over the course of the piece.

> bodyAndSoul :: Music Pitch
> bodyAndSoul =
>   tempo dhn $
>   Modify (KeySig Df Major) $
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 4 qn :+: (tempo (3 % 2) (ef 5 en :+: f 5 en :+: ef 5 en))) :+:
>   ((Modify (Phrase [Chord Bf Dom7])) $ f 5 en :+: ef 5 en :+: d 5 en :+: ef 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 5 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord D Dom7])) $ b 5 dqn :+: a 5 en) :+:

>   ((Modify (Phrase [Chord Df Maj7])) $ af 5 qn :+: (tempo (3 % 2) (af 5 en :+: bf 5 en :+: af 5 en))) :+:
>   ((Modify (Phrase [Chord Gf Dom7])) $ ef 6 dqn :+: c 6 en) :+:
>   ((Modify (Phrase [Chord F Min7])) $ ef 6 qn :+: df 6 qn) :+:
>   ((Modify (Phrase [Chord E Dim7])) $ c 6 qn :+: bf 5 qn) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ bf 4 qn :+: df 6 qn :+: (tempo (3 % 2) (bf 5 qn :+: gf 5 qn :+: bf 4 qn))) :+:
>   ((Modify (Phrase [Chord C HalfDim7])) $ f 5 hn) :+:
>   ((Modify (Phrase [Chord F Dom7])) $ ef 5 hn) :+:
>   ((Modify (Phrase [Chord Bf Min7])) $ bf 4 en :+: df 5 en) :+:
>   ((Modify (Phrase [Chord Ef Dom7])) $ ef 5 en :+: f 5 en) :+:
>   ((Modify (Phrase [Chord Ef Min7])) $ af 5 qn) :+:
>   ((Modify (Phrase [Chord Af Dom7])) $ (tempo (3 % 2) (af 5 en :+: bf 5 en :+: e 5 en))) :+:
>   ((Modify (Phrase [Chord Df Maj])) $ df 5 wn)

> bodyAndSoul' :: Music Pitch
> bodyAndSoul' =
>   tempo dhn $
>   Modify (KeySig Df Major) $
>   ((Modify (Phrase [Chord Bf Dom7])) $ f 5 hn)

> whenIFallInLove :: Music Pitch
> whenIFallInLove =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   let oct = 5
>       a = ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord F Min7])) $ ef oct hn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ bf (oct - 1) hn) :+:
>           ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord F Dom7])) $ ef oct qn :+: g oct qn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ f oct hn) :+:
>           ((Modify (Phrase [Chord Ef Maj7])) $ bf (oct - 1) qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord Af Dom7])) $ c (oct + 1) qn :+: bf oct qn) :+:
>           ((Modify (Phrase [Chord Df Dom7])) $ af oct hn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ g oct hn) :+:
>           ((Modify (Phrase [Chord F Dom7])) $ f oct hn) :+:
>           ((Modify (Phrase [Chord B Dom7])) $ f oct hn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ f oct dhn :+: g oct en :+: af oct en)
>       b = ((Modify (Phrase [Chord Ef Maj7])) $ bf oct dqn :+: ef oct en) :+:
>           ((Modify (Phrase [Chord A Dom7])) $ ef oct qn :+: ef oct qn) :+:
>           ((Modify (Phrase [Chord Af Maj7])) $ g oct qn :+: f oct qn) :+:
>           ((Modify (Phrase [Chord Df Maj7])) $ f oct qn :+: g oct en :+: af oct en) :+:
>           ((Modify (Phrase [Chord G Min7])) $ (tempo (3 % 2) (bf oct qn :+: g oct qn :+: af oct qn))) :+:
>           ((Modify (Phrase [Chord Af Maj7])) $ (tempo (3 % 2) (bf oct qn :+: g oct qn :+: af oct qn))) :+:
>           (Modify (KeySig F Minor) $ -- modulation!
>           ((Modify (Phrase [Chord G HalfDim7])) $ bf oct hn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ bf oct qn :+: af oct en :+: bf oct en))
>   in
>     a :+: b

> altTest =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   let oct = 5 in
>   ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn)

