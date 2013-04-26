Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

This file contains a few "lead sheets" to be performed by various players.

> module LeadSheets where
> import Euterpea
> import Data.Ratio


> somewhereOverTheRainbow :: Music Pitch
> somewhereOverTheRainbow =
>   tempo (7 % 8) $
>   Modify (KeySig Ef Major) $
>   let oct = 5::Octave
>       octU1 = oct + 1
>       octU2 = oct + 2
>       a' = ((Modify (Phrase [Chord Ef Maj])) $ ef oct hn) :+:
>           ((Modify (Phrase [Chord C Min7])) $ ef octU1 hn) :+:
>           ((Modify (Phrase [Chord G Min7])) $ d octU1 dqn :+: bf oct sn :+: c octU1 sn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ d octU1 qn) :+:
>           ((Modify (Phrase [Chord Ef Dom7])) $ ef octU1 qn) :+:
>           ((Modify (Phrase [Chord Af Maj7])) $ ef oct hn) :+:
>           ((Modify (Phrase [Chord A Dim7])) $ c octU1 hn) :+:
>           ((Modify (Phrase [Chord Ef Maj])) $ bf oct wn) :+:
>           ((Modify (Phrase [Chord Af Maj7])) $ c oct hn) :+:
>           ((Modify (Phrase [Chord Af Min])) $ af oct hn) :+:
>           ((Modify (Phrase [Chord G Min7])) $ g oct dqn :+: ef oct sn :+: f oct sn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ g oct qn :+: gs oct qn) :+:
>           ((Modify (Phrase [Chord F Dom7])) $ f oct dqn :+: d oct sn :+: ef oct sn) :+:
>           ((Modify (Phrase [Chord F Min7])) $ f oct qn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ g oct qn) :+:
>           ((Modify (Phrase [Chord Ef Maj])) $ ef oct wn)
>       a'' = (takeM ((dur a') - en) a') :+: bf octU1 en -- pickup to bridge!
>       b' = ((Modify (Phrase [Chord Ef Maj])) $ timesM 4 (g octU1 en :+: bf octU1 en)) :+:
>           ((Modify (Phrase [Chord F Min7])) $ timesM 2 (af octU1 en :+: bf octU1 en)) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ timesM 2 (af octU1 en :+: bf octU1 en)) :+:
>           ((Modify (Phrase [Chord G Min7])) $ c octU2 hn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ c octU2 hn) :+:
>           ((Modify (Phrase [Chord F Min7])) $ af oct hn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ bf oct dqn :+: bf octU1 en) :+:
>           ((Modify (Phrase [Chord Ef Maj])) $ timesM 4 (g octU1 en :+: bf octU1 en)) :+:
>           ((Modify (Phrase [Chord A HalfDim7])) $ timesM 2 (a octU1 en :+: c octU2 en)) :+:
>           ((Modify (Phrase [Chord D Dom7])) $ timesM 2 (a octU1 en :+: c octU2 en)) :+:
>           ((Modify (Phrase [Chord G Min7])) $ d octU2 hn) :+:
>           ((Modify (Phrase [Chord C Dom7])) $ d octU2 hn) :+:
>           ((Modify (Phrase [Chord F Min7])) $ f octU2 hn) :+:
>           ((Modify (Phrase [Chord Bf Dom7])) $ c octU2 hn)
>   in
>     a' :+: a'' :+: b' :+: a'

> somewhereOverTheRainbow' :: Music Pitch
> somewhereOverTheRainbow' =
>   let oct = 5
>       octU2 = oct + 2
>   in
>   Modify (KeySig Ef Major) $
>   tempo dhn $
>           ((Modify (Phrase [Chord C Dom7])) $ d octU2 hn)




Note that there's a cheat being used on the first note of Body and Soul, which is supposed to be a rest. In order to make the chordal texture kick in before the melody starts, I'm adding a note out of the melodic range. This musical "hack" is used several times over the course of the piece.

> bodyAndSoul :: Music Pitch
> bodyAndSoul =
>   tempo dhn $
>   Modify (KeySig Df Major) $
>   let oct = 5::Octave
>       octU1 = oct + 1
>       octD1 = oct - 1
>       a' = ((Modify (Phrase [Chord Ef Min7])) $ bf octD1 qn :+: (tempo (3 % 2) (ef oct en :+: f oct en :+: ef oct en))) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ f oct en :+: ef oct en :+: d oct en :+: ef oct en) :+:
>            ((Modify (Phrase [Chord Ef Min7])) $ bf oct qn :+: bf oct qn) :+:
>            ((Modify (Phrase [Chord D Dom7])) $ b oct dqn :+: a oct en) :+:
>            ((Modify (Phrase [Chord Df Maj7])) $ af oct qn :+: (tempo (3 % 2) (af oct en :+: bf oct en :+: af oct en))) :+:
>            ((Modify (Phrase [Chord Gf Dom7])) $ ef octU1 dqn :+: c octU1 en) :+:
>            ((Modify (Phrase [Chord F Min7])) $ ef octU1 qn :+: df octU1 qn) :+:
>            ((Modify (Phrase [Chord E Dim7])) $ c octU1 qn :+: bf oct qn) :+:
>            ((Modify (Phrase [Chord Ef Min7])) $ bf octD1 qn :+: df octU1 qn :+: (tempo (3 % 2) (bf oct qn :+: gf oct qn :+: bf octD1 qn))) :+:
>            ((Modify (Phrase [Chord C HalfDim7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord F Dom7])) $ f oct qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord Bf Min7])) $ bf octD1 en :+: df oct en) :+:
>            ((Modify (Phrase [Chord Ef Dom7])) $ ef oct en :+: f oct en) :+:
>            ((Modify (Phrase [Chord Ef Min7])) $ af oct qn) :+:
>            ((Modify (Phrase [Chord Af Dom7])) $ (tempo (3 % 2) (af oct en :+: bf oct en :+: e oct en))) :+:
>            ((Modify (Phrase [Chord Df Maj])) $ df oct wn)
>       a''  = (takeM ((dur a') - hn) a') :+:
>            ((Modify (Phrase [Chord F HalfDim7])) $ b octD1 qn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ bf octD1 qn)
>       a''' = (takeM ((dur a') - hn) a') :+:
>            ((Modify (Phrase [Chord E HalfDim7])) $ c oct qn) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ bf octD1 qn)
>       b'   = Modify (KeySig D Major) $
>            ((Modify (Phrase [Chord D Maj7])) $ a octD1 en :+: d oct en :+: e oct en :+: fs oct en) :+:
>            ((Modify (Phrase [Chord E Min7])) $ a oct en :+: a oct qn :+: a oct en) :+:
>            ((Modify (Phrase [Chord D Maj7])) $ d octU1 en :+: d octU1 qn :+: fs oct en) :+:
>            ((Modify (Phrase [Chord G Min7])) $ a oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ g oct qn) :+:
>            ((Modify (Phrase [Chord Fs Min7])) $ fs oct qn) :+:
>            ((Modify (Phrase [Chord B Min7])) $ d oct qn) :+:
>            ((Modify (Phrase [Chord E Min7])) $ e oct qn) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ cs oct qn) :+:
>            ((Modify (Phrase [Chord D Maj7])) $ a octD1 wn)
>       b''  = Modify (KeySig C Major) $
>            ((Modify (Phrase [Chord D Min7])) $ a octD1 en :+: d oct en :+: e oct en :+: f oct en) :+:
>            ((Modify (Phrase [Chord G Dom7])) $ a oct en :+: a oct qn :+: g oct en) :+:
>            ((Modify (Phrase [Chord C Maj7])) $ e octU1 en :+: e octU1 qn :+: b oct en) :+:
>            ((Modify (Phrase [Chord Ef Dim7])) $ d octU1 en :+: d octU1 qn :+: a oct en) :+:
>            ((Modify (Phrase [Chord D Min7])) $ c octU1 en :+: c octU1 qn :+: a oct en) :+:
>            ((Modify (Phrase [Chord G Dom7])) $ b oct qn :+: g oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ e oct qn) :+:
>            ((Modify (Phrase [Chord B Dom7])) $ ds oct qn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ d oct qn :+: bf oct qn)
>   in
>     a'' :+: a''' :+: b' :+: b'' :+: a'

> bodyAndSoul' :: Music Pitch
> bodyAndSoul' =
>   let oct = 5 in
>   tempo dhn $
>           Modify (KeySig Df Major) $
>           ((Modify (Phrase [Chord C HalfDim7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord F Dom7])) $ f oct qn :+: ef oct qn)


> whenIFallInLove :: Music Pitch
> whenIFallInLove =
>   tempo dhn $
>   Modify (KeySig Ef Major) $
>   let oct = 5
>       octU1 = oct + 1
>       octD1 = oct - 1
>       a' = ((Modify (Phrase [Chord Ef Maj7])) $ bf octD1 qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>            ((Modify (Phrase [Chord F Min7])) $ ef oct hn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ bf octD1 hn) :+:
>            ((Modify (Phrase [Chord Ef Maj7])) $ bf octD1 qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>            ((Modify (Phrase [Chord F Dom7])) $ ef oct hn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord Ef Maj7])) $ bf octD1 qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord Af Dom7])) $ c octU1 qn :+: bf oct qn) :+:
>            ((Modify (Phrase [Chord Df Dom7])) $ af oct hn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ g oct hn) :+:
>            ((Modify (Phrase [Chord F Dom7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord B Dom7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ f oct dhn :+: g oct en :+: af oct en)
>       b' = ((Modify (Phrase [Chord Ef Maj7])) $ bf oct dqn :+: ef oct en) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ ef oct qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord Af Maj7])) $ g oct qn :+: f oct qn) :+:
>            ((Modify (Phrase [Chord Df Maj7])) $ f oct qn :+: g oct en :+: af oct en) :+:
>            ((Modify (Phrase [Chord G Min7])) $ (tempo (3 % 2) (bf oct qn :+: g oct qn :+: af oct qn))) :+:
>            ((Modify (Phrase [Chord Af Maj7])) $ (tempo (3 % 2) (bf oct qn :+: g oct qn :+: af oct qn))) :+:
>            (Modify (KeySig F Minor) $ -- modulation!
>            ((Modify (Phrase [Chord G HalfDim7])) $ bf oct hn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ bf oct qn :+: af oct en :+: bf oct en) :+:
>            ((Modify (Phrase [Chord F Min])) $ c octU1 dqn :+: f oct en) :+:
>            ((Modify (Phrase [Chord B Dom7])) $ f oct qn :+: f oct qn) :+:
>            ((Modify (Phrase [Chord Df Dom7])) $ af oct qn :+: g oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ g oct qn :+: af oct en :+: bf oct en) :+:
>            ((Modify (Phrase [Chord F Min7])) $ (tempo (3 % 2) (c octU1 qn :+: af oct qn :+: bf oct qn))) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ (tempo (3 % 2) (c octU1 qn :+: af oct qn :+: c octU1 qn)))) :+:
>            (Modify (KeySig Ef Major) $ -- and back
>            ((Modify (Phrase [Chord F Min7])) $ bf oct hn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ bf oct hn))
>       c' = ((Modify (Phrase [Chord Ef Maj7])) $ bf oct dqn :+: ef oct en) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ ef oct qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord Af Maj7])) $ d octU1 qn :+: c octU1 qn) :+:
>            ((Modify (Phrase [Chord D Dom7])) $ c octU1 qn :+: c oct qn) :+:
>            ((Modify (Phrase [Chord G Min7])) $ c octU1 qn :+: bf oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ bf oct qn :+: ef octU1 qn) :+:
>            ((Modify (Phrase [Chord F Min7])) $ af oct hn) :+:
>            ((Modify (Phrase [Chord Df Dom7])) $ af oct qn :+: f oct qn) :+:
>            ((Modify (Phrase [Chord Ef Maj7])) $ bf octD1 qn :+: ef oct qn) :+:
>            ((Modify (Phrase [Chord C Dom7])) $ af oct qn :+: g oct qn) :+:
>            ((Modify (Phrase [Chord B Dom7])) $ ef oct hn) :+:
>            ((Modify (Phrase [Chord Bf Dom7])) $ f oct hn) :+:
>            ((Modify (Phrase [Chord Ef Maj])) $ ef oct dwn)
>   in
>     a' :+: b' :+: a' :+: c'

> whenIFallInLove' :: Music Pitch
> whenIFallInLove' =
>   let oct = 5 in
>   tempo dhn $
>   Modify (KeySig F Minor) $
>            ((Modify (Phrase [Chord Df Dom7])) $ af oct wn)

> blackOrpheus :: Music Pitch
> blackOrpheus =
>   Modify (Tempo (5 % 4)) $
>   Modify (KeySig A Minor) $
>   let oct = 5
>       octU1 = oct + 1
>       octD1 = oct - 1
>       pickup = e oct qn
>       a' = ((Modify (Phrase [Chord A Min])) $ c octU1 dhn :+: b oct en :+: a oct en) :+:
>            ((Modify (Phrase [Chord B HalfDim7])) $ a oct hn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ gs oct qn :+: b oct qn) :+:
>            ((Modify (Phrase [Chord A Min])) $ e oct wn) :+:
>            ((Modify (Phrase [Chord B HalfDim7])) $ e oct hn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ e oct hn)
>       b' =   Modify (KeySig C Major) $
>            ((Modify (Phrase [Chord A Min])) $ c octU1 dhn :+: b oct en :+: a oct en) :+:
>            ((Modify (Phrase [Chord D Min7])) $ a oct hn) :+:
>            ((Modify (Phrase [Chord G Dom7])) $ g oct qn :+: b oct qn) :+:
>            ((Modify (Phrase [Chord C Maj])) $ e oct wn) :+:
>            ((Modify (Phrase [Chord Cs Dim7])) $ e oct hn) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ e oct qn :+: f oct en :+: g oct en) :+:
>            ((Modify (Phrase [Chord D Min7])) $ a oct dqn :+: d oct en :+: d oct hn) :+:
>            ((Modify (Phrase [Chord G Dom7])) $ d oct hn :+: d oct qn :+: e oct en :+: f oct en) :+:
>            ((Modify (Phrase [Chord C Maj])) $ g oct dqn :+: c oct en :+: c oct hn) :+:
>            ((Modify (Phrase [Chord F Maj7])) $ c oct hn :+: c oct qn :+: d oct en :+: e oct en) :+:
>            (Modify (KeySig A Minor) $
>            ((Modify (Phrase [Chord B HalfDim7])) $ f oct dqn :+: b octD1 en :+: b octD1 hn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ b octD1 hn :+: b octD1 qn :+: c oct en :+: d oct en) :+:
>            ((Modify (Phrase [Chord A Min])) $ e oct wn) :+:
>            ((Modify (Phrase [Chord B HalfDim7])) $ e oct hn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ e oct hn))
>       c' =   Modify (KeySig D Minor) $
>            ((Modify (Phrase [Chord E HalfDim7])) $ bf oct dhn :+: a oct en :+: g oct en) :+:
>            ((Modify (Phrase [Chord A Dom7])) $ g oct dhn :+: f oct en :+: e oct en) :+:
>            ((Modify (Phrase [Chord D Min])) $ a oct dwn :+: d oct hn) :+:
>            (Modify (KeySig A Minor) $
>            ((Modify (Phrase [Chord D Min7])) $ d oct dhn :+: e oct en :+: f oct en) :+:
>            ((Modify (Phrase [Chord B HalfDim7])) $ b oct hn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ b oct hn) :+:
>            ((Modify (Phrase [Chord A Min7])) $ c oct hn :+: c oct qn :+: d oct en :+: e oct en) :+:
>            ((Modify (Phrase [Chord F Maj7])) $ a oct dhn :+: gs oct qn) :+:
>            ((Modify (Phrase [Chord B HalfDim7])) $ e oct wn) :+:
>            ((Modify (Phrase [Chord E Dom7])) $ e oct hn :+: e oct qn :+: gs oct en :+: b oct en) :+:
>            ((Modify (Phrase [Chord A Min])) $ a oct dwn))
>   in
>     pickup :+: a' :+: b' :+: a' :+: c'


> blackOrpheus' :: Music Pitch
> blackOrpheus' =
>   let oct = 5 in
>   tempo dhn $
>   Modify (KeySig C Major) $
>            ((Modify (Phrase [Chord C Maj7])) $ e oct wn)
