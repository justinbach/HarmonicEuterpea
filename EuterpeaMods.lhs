Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

This file contains exports core Euterpea functionality for use in other modules, as well as the modifications I made to support harmonic context.

> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

> module EuterpeaMods (module EuterpeaMods, module Euterpea) where
> import Codec.Midi
> import Sound.PortMidi
> import Control.Exception (finally)
> import Control.Concurrent
> import Control.Concurrent.STM.TChan
> import Control.Monad.STM (atomically)
> import Data.IORef

> import Data.Bits (shiftR, shiftL, (.|.), (.&.))
> import Data.List (findIndex)
> import Data.List(partition)
> import Data.Char(toLower,toUpper)
> import qualified Data.Heap as Heap

> import System.IO (hPutStrLn, stderr)
> import System.IO.Unsafe (unsafePerformIO)
> import System.Random
> import Euterpea hiding (

definitions from music.hs:

>   Octave,
>   Pitch,
>   Dur,
>   PitchClass(..),
>   Primitive(..),
>   Music(..),
>   Control(..),
>   PlayerName,
>   Mode(..),
>   InstrumentName(..),
>   PhraseAttribute(..),
>   Dynamic(..),
>   StdLoudness(..),
>   Tempo(..),
>   Articulation(..),
>   Ornament(..),
>   NoteHead(..),
>   note, rest, tempo, transpose, instrument, phrase, player, keysig,
>   cff, cf, c, cs, css, dff, df, d, ds, dss, eff, ef, e, es, ess,
>   fff, ff, f, fs, fss, gff, gf, g, gs, gss, aff, af, a, as, ass,
>   bff, bf, b, bs, bss,
>   bn, bnr, wn, wnr, hn, hnr, qn, qnr, en, enr, sn, snr, tn, tnr,
>   sfn, sfnr, dwn, dwnr, dhn, dhnr, dqn, dqnr, den, denr, dsn, dsnr,
>   dtn, dtnr, ddhn, ddhnr, ddqn, ddqnr, dden, ddenr,
>   AbsPitch,
>   absPitch, pcToInt, pitch, trans,

definitions from performance.hs:

>   Performance,
>   Event(..),
>   PTime,
>   DurT,
>   Context(..),
>   metro,
>   PMap,
>   merge, perform, perf,
>   Note1,
>   Music1,
>   toMusic1, toMusic1',
>   Player(..),
>   NoteFun,
>   PhraseFun,
>   NotateFun,
>   defPlayer, defPlayNote, defNasHandler,
>   defInterpPhrase, defPasHandler, defPMap, defCon,
>   fancyPlayer, fancyInterpPhrase,
>   Performable,
>   perfDur,
>   defToPerf, toPerf,

definitions from moremusic.lhs:

>   dur, takeM, dropM, timesM, repeatM, delayM, mMap, pMap,
>   line, chord,

definitions from tomidi.hs:

>   toMidi, UserPatchMap, defST, defUpm, testMidi, testMidiA,
>   test, testA, play, playM, playA, makeMidi, mToMF, gmUpm,
>   gmTest,

definitions from generalmidi.hs

>   fromGM, toGM,

definitions from midiio.lhs

>   defaultOutput, defaultInput, playMidi,
>   MidiMessage (ANote, Std),
>   DeviceInfo(..), DeviceID, Message(..), Time

>   )

> infixr 5 :+:, :=:

BEGIN CUSTOM MODIFICATIONS

Support for harmonic context as a PhraseAttribute:

> data PhraseAttribute  =  Dyn Dynamic
>                       |  Tmp EuterpeaMods.Tempo
>                       |  Art Articulation
>                       |  Orn Ornament
>                       |  Chord PitchClass ChordType
>      deriving (Eq, Ord, Show)

Support for different chord types. This set consists of all possible triads
and seventh chords obtainable by building on each degree of the major and
harmonic minor scales.

> data ChordType =  Maj | Min | Dim | Aug | Maj7 | Min7 | Dom7 | HalfDim7 | Dim7 | MinMaj7 | AugMaj7
>     deriving (Eq, Ord, Show)

State monad for random number generation:

> data SM a = SM (StdGen -> (StdGen, a))

> instance Monad SM where
>   return a = SM $ (\s0 -> (s0, a))
>   SM sm0 >>= fsm1
>     = SM $ \s0 ->
>       let (s1, a1) = sm0 s0
>           SM sm1   = fsm1 a1
>           (s2, a2) = sm1 s1
>       in (s2, a2)

Revised versions of performance-related functions to support the monad:

> perf :: PMap a -> Context a -> Music a -> SM (Performance, DurT)
> perf pm
>   c@Context {cTime = t, cPlayer = pl, cDur = dt, cPch = k} m =
>   case m of
>      Prim (Note d p)            -> return (playNote pl c d p, d*dt)
>      Prim (Rest d)              -> return ([], d*dt)
>      m1 :+: m2                  ->
>              do (pf1, d1) <- perf pm c m1
>                 (pf2, d2) <- perf pm (c {cTime = t + d1}) m2
>                 return (pf1++pf2, d1+d2)
>      m1 :=: m2                  ->
>              do (pf1, d1) <- perf pm c m1
>                 (pf2, d2) <- perf pm (c {cTime = t + d1}) m2
>                 return (EuterpeaMods.merge pf1 pf2, max d1 d2)
>      Modify  (Tempo r)       m  -> perf pm (c {cDur = dt / r})    m
>      Modify  (Transpose p)   m  -> perf pm (c {cPch = k + p})     m
>      Modify  (Instrument i)  m  -> perf pm (c {cInst = i})        m
>      Modify  (KeySig pc mo)  m  -> perf pm (c {cKey = (pc,mo)})   m
>      Modify  (Player pn)     m  -> perf pm (c {cPlayer = pm pn})  m
>      Modify  (Phrase pas)    m  -> interpPhrase pl pm c pas       m

> perform :: PMap a -> Context a -> Music a -> Performance

> perform pm c m = let SM f = perf pm c m in
>                  fst $ snd (f (mkStdGen 50))

> type PhraseFun a  =  PMap a -> Context a -> [PhraseAttribute]
>                      -> Music a -> SM (Performance, DurT)

BEGIN MUSIC.HS DEFINITIONS

> type Octave = Int
> type Pitch = (PitchClass, Octave)
> type Dur   = Rational
> data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
>                  |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
>                  |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
>                  |  Bf | Ass | B | Bs | Bss
>      deriving (Eq, Ord, Show, Read, Enum)
> data Primitive a  =  Note Dur a
>                   |  Rest Dur
>      deriving (Show, Eq, Ord)
> data Music a  =
>        Prim (Primitive a)               -- primitive value
>     |  Music a :+: Music a              -- sequential composition
>     |  Music a :=: Music a              -- parallel composition
>     |  Modify Control (Music a)         -- modifier
>   deriving (Show, Eq, Ord)
> data Control =
>           Tempo       Rational           -- scale the tempo
>        |  Transpose   AbsPitch           -- transposition
>        |  Instrument  InstrumentName     -- instrument label
>        |  Phrase      [PhraseAttribute]  -- phrase attributes
>        |  Player      PlayerName         -- player label
>        |  KeySig      PitchClass Mode    -- key signature and mode
>   deriving (Show, Eq, Ord)
>
> type PlayerName  = String
> data Mode        = Major | Minor
>   deriving (Eq, Ord, Show)
> data InstrumentName =
>      AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
>   |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
>   |  Harpsichord            | Clavinet               | Celesta
>   |  Glockenspiel           | MusicBox               | Vibraphone
>   |  Marimba                | Xylophone              | TubularBells
>   |  Dulcimer               | HammondOrgan           | PercussiveOrgan
>   |  RockOrgan              | ChurchOrgan            | ReedOrgan
>   |  Accordion              | Harmonica              | TangoAccordion
>   |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
>   |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
>   |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
>   |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
>   |  SlapBass1              | SlapBass2              | SynthBass1
>   |  SynthBass2             | Violin                 | Viola
>   |  Cello                  | Contrabass             | TremoloStrings
>   |  PizzicatoStrings       | OrchestralHarp         | Timpani
>   |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
>   |  SynthStrings2          | ChoirAahs              | VoiceOohs
>   |  SynthVoice             | OrchestraHit           | Trumpet
>   |  Trombone               | Tuba                   | MutedTrumpet
>   |  FrenchHorn             | BrassSection           | SynthBrass1
>   |  SynthBrass2            | SopranoSax             | AltoSax
>   |  TenorSax               | BaritoneSax            | Oboe
>   |  Bassoon                | EnglishHorn            | Clarinet
>   |  Piccolo                | Flute                  | Recorder
>   |  PanFlute               | BlownBottle            | Shakuhachi
>   |  Whistle                | Ocarina                | Lead1Square
>   |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
>   |  Lead5Charang           | Lead6Voice             | Lead7Fifths
>   |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
>   |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
>   |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
>   |  FX1Train               | FX2Soundtrack          | FX3Crystal
>   |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
>   |  FX7Echoes              | FX8SciFi               | Sitar
>   |  Banjo                  | Shamisen               | Koto
>   |  Kalimba                | Bagpipe                | Fiddle
>   |  Shanai                 | TinkleBell             | Agogo
>   |  SteelDrums             | Woodblock              | TaikoDrum
>   |  MelodicDrum            | SynthDrum              | ReverseCymbal
>   |  GuitarFretNoise        | BreathNoise            | Seashore
>   |  BirdTweet              | TelephoneRing          | Helicopter
>   |  Applause               | Gunshot                | Percussion
>   |  Custom String
>   deriving (Show, Eq, Ord)
> data Dynamic  =  Accent Rational | Crescendo Rational | Diminuendo Rational
>               |  StdLoudness StdLoudness | Loudness Rational
>      deriving (Eq, Ord, Show)
>
> data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF
>      deriving (Eq, Ord, Show, Enum)
>
> data Tempo = Ritardando Rational | Accelerando Rational
>      deriving (Eq, Ord, Show)
>
> data Articulation  =  Staccato Rational | Legato Rational | Slurred Rational
>                    |  Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
>                    |  DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
>                    |  BartokPizz | Swell | Wedge | Thumb | Stopped
>      deriving (Eq, Ord, Show)
>
> data Ornament  =  Trill | Mordent | InvMordent | DoubleMordent
>                |  Turn | TrilledTurn | ShortTrill
>                |  Arpeggio | ArpeggioUp | ArpeggioDown
>                |  Instruction String | Head NoteHead
>                |  DiatonicTrans Int
>      deriving (Eq, Ord, Show)
>
> data NoteHead  =  DiamondHead | SquareHead | XHead | TriangleHead
>                |  TremoloHead | SlashHead | ArtHarmonic | NoHead
>      deriving (Eq, Ord, Show)
>
> note            :: Dur -> a -> Music a
> note d p        = Prim (Note d p)
>
> rest            :: Dur -> Music a
> rest d          = Prim (Rest d)
>
> tempo           :: Dur -> Music a -> Music a
> tempo r m       = Modify (Tempo r) m
>
> transpose       :: AbsPitch -> Music a -> Music a
> transpose i m   = Modify (Transpose i) m
>
> instrument      :: InstrumentName -> Music a -> Music a
> instrument i m  = Modify (Instrument i) m
>
> phrase          :: [PhraseAttribute] -> Music a -> Music a
> phrase pa m     = Modify (Phrase pa) m
>
> player          :: PlayerName -> Music a -> Music a
> player pn m     = Modify (Player pn) m
>
> keysig          :: PitchClass -> Mode -> Music a -> Music a
> keysig pc mo m  = Modify (KeySig pc mo) m
> cff,cf,c,cs,css,dff,df,d,ds,dss,eff,ef,e,es,ess,fff,ff,f,
>   fs,fss,gff,gf,g,gs,gss,aff,af,a,as,ass,bff,bf,b,bs,bss ::
>     Octave -> Dur -> Music Pitch
>
> cff  o d = note d (Cff,  o);  cf   o d = note d (Cf,   o)
> c    o d = note d (C,    o);  cs   o d = note d (Cs,   o)
> css  o d = note d (Css,  o);  dff  o d = note d (Dff,  o)
> df   o d = note d (Df,   o);  d    o d = note d (D,    o)
> ds   o d = note d (Ds,   o);  dss  o d = note d (Dss,  o)
> eff  o d = note d (Eff,  o);  ef   o d = note d (Ef,   o)
> e    o d = note d (E,    o);  es   o d = note d (Es,   o)
> ess  o d = note d (Ess,  o);  fff  o d = note d (Fff,  o)
> ff   o d = note d (Ff,   o);  f    o d = note d (F,    o)
> fs   o d = note d (Fs,   o);  fss  o d = note d (Fss,  o)
> gff  o d = note d (Gff,  o);  gf   o d = note d (Gf,   o)
> g    o d = note d (G,    o);  gs   o d = note d (Gs,   o)
> gss  o d = note d (Gss,  o);  aff  o d = note d (Aff,  o)
> af   o d = note d (Af,   o);  a    o d = note d (A,    o)
> as   o d = note d (As,   o);  ass  o d = note d (Ass,  o)
> bff  o d = note d (Bff,  o);  bf   o d = note d (Bf,   o)
> b    o d = note d (B,    o);  bs   o d = note d (Bs,   o)
> bss  o d = note d (Bss,  o)
> bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn,
>     dqn, den, dsn, dtn, ddhn, ddqn, dden :: Dur
>
> bnr, wnr, hnr, qnr, enr, snr, tnr, dwnr, dhnr,
>      dqnr, denr, dsnr, dtnr, ddhnr, ddqnr, ddenr :: Music Pitch
>
> bn    = 2;     bnr    = rest bn    -- brevis rest
> wn    = 1;     wnr    = rest wn    -- whole note rest
> hn    = 1/2;   hnr    = rest hn    -- half note rest
> qn    = 1/4;   qnr    = rest qn    -- quarter note rest
> en    = 1/8;   enr    = rest en    -- eighth note rest
> sn    = 1/16;  snr    = rest sn    -- sixteenth note rest
> tn    = 1/32;  tnr    = rest tn    -- thirty-second note rest
> sfn   = 1/64;  sfnr   = rest sfn   -- sixty-fourth note rest
>
> dwn   = 3/2;   dwnr   = rest dwn   -- dotted whole note rest
> dhn   = 3/4;   dhnr   = rest dhn   -- dotted half note rest
> dqn   = 3/8;   dqnr   = rest dqn   -- dotted quarter note rest
> den   = 3/16;  denr   = rest den   -- dotted eighth note rest
> dsn   = 3/32;  dsnr   = rest dsn   -- dotted sixteenth note rest
> dtn   = 3/64;  dtnr   = rest dtn   -- dotted thirty-second note rest
>
> ddhn  = 7/8;   ddhnr  = rest ddhn  -- double-dotted half note rest
> ddqn  = 7/16;  ddqnr  = rest ddqn  -- double-dotted quarter note rest
> dden  = 7/32;  ddenr  = rest dden  -- double-dotted eighth note restt251  :: Music Pitch
> t251  =  let  dMinor  = d 4 wn  :=: f 4 wn  :=: a 4 wn
>               gMajor  = g 4 wn  :=: b 4 wn  :=: d 5 wn
>               cMajor  = c 4 bn  :=: e 4 bn  :=: g 4 bn
>          in dMinor :+: gMajor :+: cMajor
> type AbsPitch = Int
> absPitch           :: Pitch -> AbsPitch
> absPitch (pc,oct)  = 12*oct + pcToInt pc
> pcToInt     :: PitchClass -> Int
> pcToInt pc  = case pc of
>   Cff  -> -2;  Cf  -> -1;  C  -> 0;   Cs  -> 1;   Css  -> 2;
>   Dff  -> 0;   Df  -> 1;   D  -> 2;   Ds  -> 3;   Dss  -> 4;
>   Eff  -> 2;   Ef  -> 3;   E  -> 4;   Es  -> 5;   Ess  -> 6;
>   Fff  -> 3;   Ff  -> 4;   F  -> 5;   Fs  -> 6;   Fss  -> 7;
>   Gff  -> 5;   Gf  -> 6;   G  -> 7;   Gs  -> 8;   Gss  -> 9;
>   Aff  -> 7;   Af  -> 8;   A  -> 9;   As  -> 10;  Ass  -> 11;
>   Bff  -> 9;   Bf  -> 10;  B  -> 11;  Bs  -> 12;  Bss  -> 13
> pitch     :: AbsPitch -> Pitch
> pitch ap  =
>     let (oct, n) = divMod ap 12
>     in  ([C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n, oct)
> trans      :: Int -> Pitch -> Pitch
> trans i p  = pitch (absPitch p + i)

BEGIN PERFORMANCE.HS DEFINITIONS

> type Performance = [Event]
>
> data Event = Event {  eTime    :: PTime,
>                       eInst    :: InstrumentName,
>                       ePitch   :: AbsPitch,
>                       eDur     :: DurT,
>                       eVol     :: Volume,
>                       eParams  :: [Double]}
>      deriving (Eq,Ord,Show)
> type PTime     = Rational
> type DurT      = Rational
> data Context a = Context {  cTime    :: PTime,
>                             cPlayer  :: Player a,
>                             cInst    :: InstrumentName,
>                             cDur     :: DurT,
>                             cPch     :: AbsPitch,
>                             cVol     :: Volume,
>                             cKey     :: (PitchClass, Mode) }
>      deriving Show
> metro              :: Int -> Dur -> DurT
> metro setting dur  = 60 / (fromIntegral setting * dur)
>
> type PMap a  = PlayerName -> Player a
>
> merge []          es2         =  es2
> merge es1         []          =  es1
> merge a@(e1:es1)  b@(e2:es2)  =
>   if eTime e1 < eTime e2  then  e1  : EuterpeaMods.merge es1 b
>                           else  e2  : EuterpeaMods.merge a es2
>
>

> type Note1   = (Pitch, [NoteAttribute])
> type Music1  = Music Note1
>
> toMusic1   :: Music Pitch -> Music1
> toMusic1   = mMap (\p -> (p, []))
>
> toMusic1'  :: Music (Pitch, Volume) -> Music1
> toMusic1'  = mMap (\(p, v) -> (p, [Volume v]))
> data Player a = MkPlayer {  pName         :: PlayerName,
>                             playNote      :: NoteFun a,
>                             interpPhrase  :: PhraseFun a,
>                             notatePlayer  :: NotateFun a }
>
> type NoteFun a    =  Context a -> Dur -> a -> Performance
> type NotateFun a  =  ()
>
> instance Show a => Show (Player a) where
>    show p = "Player " ++ pName p
> defPlayer  :: Player Note1
> defPlayer  = MkPlayer
>              {  pName         = "Default",
>                 playNote      = defPlayNote      defNasHandler,
>                 interpPhrase  = defInterpPhrase  defPasHandler,
>                 notatePlayer  = () }
> defPlayNote ::  (Context (Pitch,[a]) -> a -> Event-> Event)
>                 -> NoteFun (Pitch, [a])
> defPlayNote nasHandler
>   c@(Context cTime cPlayer cInst cDur cPch cVol cKey) d (p,nas) =
>     let initEv = Event {  eTime    = cTime,     eInst  = cInst,
>                           eDur     = d * cDur,  eVol = cVol,
>                           ePitch   = absPitch p + cPch,
>                           eParams  = [] }
>     in [ foldr (nasHandler c) initEv nas ]
>
> defNasHandler :: Context a -> NoteAttribute -> Event -> Event
> defNasHandler c (Volume v)     ev = ev {eVol = v}
> defNasHandler c (Params pms)   ev = ev {eParams = pms}
> defNasHandler _            _   ev = ev
>
> defInterpPhrase ::
>    (PhraseAttribute -> Performance -> Performance) ->
>    (  PMap a -> Context a -> [PhraseAttribute] ->  --PhraseFun
>       Music a -> SM (Performance, DurT) )
> defInterpPhrase pasHandler pm context pas m =
>        do (pf,dur) <- perf pm context m
>           return (foldr pasHandler pf pas, dur)
>
> defPasHandler :: PhraseAttribute -> Performance -> Performance
> defPasHandler (Dyn (Accent x))    =
>     map (\e -> e {eVol = round (x * fromIntegral (eVol e))})
> defPasHandler (Art (Staccato x))  =
>     map (\e -> e {eDur = x * eDur e})
> defPasHandler (Art (Legato   x))  =
>     map (\e -> e {eDur = x * eDur e})
> defPasHandler _                   = id
>
> defPMap            :: PMap Note1
> defPMap "Fancy"    = fancyPlayer
> defPMap "Default"  = defPlayer
> defPMap n          = defPlayer { pName = n }
>
> defCon  :: Context Note1
> defCon  = Context {  cTime    = 0,
>                      cPlayer  = fancyPlayer,
>                      cInst    = AcousticGrandPiano,
>                      cDur     = metro 120 qn,
>                      cPch     = 0,
>                      cKey     = (C, Major),
>                      cVol     = 127 }
>
> fancyPlayer :: Player (Pitch, [NoteAttribute])
> fancyPlayer  = MkPlayer {  pName         = "Fancy",
>                            playNote      = defPlayNote defNasHandler,
>                            interpPhrase  = fancyInterpPhrase,
>                            notatePlayer  = () }
>
> fancyInterpPhrase             :: PhraseFun a
> fancyInterpPhrase pm c [] m   = perf pm c m
> fancyInterpPhrase pm
>   c@Context {  cTime = t, cPlayer = pl, cInst = i,
>                cDur = dt, cPch = k, cVol = v}
>   (pa:pas) m =
>   let  pfd@(pf,dur)  =  fancyInterpPhrase pm c pas m
>        loud x        =  fancyInterpPhrase pm c (Dyn (Loudness x) : pas) m
>        stretch x     =  let  t0 = eTime (head pf);  r  = x/dur
>                              upd (e@Event {eTime = t, eDur = d}) =
>                                let  dt  = t-t0
>                                     t'  = (1+dt*r)*dt + t0
>                                     d'  = (1+(2*dt+d)*r)*d
>                                in e {eTime = t', eDur = d'}
>                         in return (map upd pf, (1+x)*dur)
>        inflate x     =  let  t0  = eTime (head pf);
>                              r   = x/dur
>                              upd (e@Event {eTime = t, eVol = v}) =
>                                  e {eVol =  round ( (1+(t-t0)*r) *
>                                             fromIntegral v)}
>                         in return (map upd pf, dur)
>   in case pa of
>     Dyn (Accent x) ->
>         return (map (\e-> e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
>     Dyn (StdLoudness l) ->
>         case l of
>            PPP  -> loud 40;       PP -> loud 50;   P    -> loud 60
>            MP   -> loud 70;       SF -> loud 80;   MF   -> loud 90
>            NF   -> loud 100;      FF -> loud 110;  FFF  -> loud 120
>     Dyn (Loudness x)     ->  fancyInterpPhrase pm
>                              c{cVol = (round . fromRational) x} pas m
>     Dyn (Crescendo x)    ->  inflate   x ; Dyn (Diminuendo x)  -> inflate (-x)
>     Tmp (Ritardando x)   ->  stretch   x ; Tmp (Accelerando x) -> stretch (-x)
>     Art (Staccato x)     ->  return (map (\e-> e {eDur = x * eDur e}) pf, dur)
>     Art (Legato x)       ->  return (map (\e-> e {eDur = x * eDur e}) pf, dur)
>     Art (Slurred x)      ->
>         let  lastStartTime  = foldr (\e t -> max (eTime e) t) 0 pf
>              setDur e       =   if eTime e < lastStartTime
>                                 then e {eDur = x * eDur e}
>                                 else e
>         in return (map setDur pf, dur)
>     Art _                -> pfd
>     Orn _                -> pfd
>
> class Performable a where
>   perfDur :: PMap Note1 -> Context Note1 -> Music a -> (Performance, DurT)
>
> instance Performable Note1 where
>   perfDur pm c m = let SM f = perf pm c m in
>                    snd $ (f (mkStdGen 50))
>
> instance Performable Pitch where
>   perfDur pm c = perfDur pm c . toMusic1
>
> instance Performable (Pitch, Volume) where
>   perfDur pm c = perfDur pm c . toMusic1'
>
> defToPerf :: Performable a => Music a -> Performance
> defToPerf = fst . perfDur defPMap defCon
>
> toPerf :: Performable a => PMap Note1 -> Context Note1 -> Music a ->  Performance
> toPerf pm con = fst . perfDur pm con




BEGIN MOREMUSIC.LHS DEFINITIONS

> line, chord :: [Music a] -> Music a
> line   = foldr (:+:) (rest 0)
> chord  = foldr (:=:) (rest 0)

> dur                       :: Music a -> Dur
> dur (Prim (Note d _))     = d
> dur (Prim (Rest d))       = d
> dur (m1 :+: m2)           = dur m1   +   dur m2
> dur (m1 :=: m2)           = dur m1 `max` dur m2
> dur (Modify (Tempo r) m)  = dur m / r
> dur (Modify _ m)          = dur m

> takeM :: Dur -> Music a -> Music a
> takeM d m | d <= 0            = rest 0
> takeM d (Prim (Note oldD p))  = note (min oldD d) p
> takeM d (Prim (Rest oldD))    = rest (min oldD d)
> takeM d (m1 :=: m2)           = takeM d m1 :=: takeM d m2
> takeM d (m1 :+: m2)           =  let  m'1  = takeM d m1
>                                       m'2  = takeM (d - dur m'1) m2
>                                  in m'1 :+: m'2
> takeM d (Modify (Tempo r) m)  = tempo r (takeM (d*r) m)
> takeM d (Modify c m)          = Modify c (takeM d m)

> dropM :: Dur -> Music a -> Music a
> dropM d m | d <= 0            = m
> dropM d (Prim (Note oldD p))  = note (max (oldD-d) 0) p
> dropM d (Prim (Rest oldD))    = rest (max (oldD-d) 0)
> dropM d (m1 :=: m2)           = dropM d m1 :=: dropM d m2
> dropM d (m1 :+: m2)           =  let  m'1  = dropM d m1
>                                       m'2  = dropM (d - dur m1) m2
>                                  in m'1 :+: m'2
> dropM d (Modify (Tempo r) m)  = tempo r (dropM (d*r) m)
> dropM d (Modify c m)          = Modify c (dropM d m)

> delayM      :: Dur -> Music a -> Music a
> delayM d m  = rest d :+: m

> timesM      :: Int -> Music a -> Music a
> timesM 0 m  = rest 0
> timesM n m  = m :+: timesM (n-1) m

> repeatM    :: Music a -> Music a
> repeatM m  = m :+: repeatM m

> pMap               :: (a -> b) -> Primitive a -> Primitive b
> pMap f (Note d x)  = Note d (f x)
> pMap f (Rest d)    = Rest d

> mMap                 :: (a -> b) -> Music a -> Music b
> mMap f (Prim p)      = Prim (pMap f p)
> mMap f (m1 :+: m2)   = mMap f m1 :+: mMap f m2
> mMap f (m1 :=: m2)   = mMap f m1 :=: mMap f m2
> mMap f (Modify c m)  = Modify c (mMap f m)

BEGIN DEFINITIONS FROM TOMIDI.HS

> type ProgNum     = Int
> type UserPatchMap = [(InstrumentName, Channel)]
> makeGMMap :: [InstrumentName] -> UserPatchMap
> makeGMMap ins = mkGMMap 0 ins
>   where mkGMMap _ []        = []
>         mkGMMap n _ | n>=15 =
>                   error "MakeGMMap: Too many instruments."
>         mkGMMap n (Percussion : ins)    =
>                   (Percussion, 9) : mkGMMap n ins
>         mkGMMap n (i : ins) =
>                   (i, chanList !! n) : mkGMMap (n+1) ins
>         chanList = [0..8] ++ [10..15]  -- channel 9 is for percussion
> upmLookup :: UserPatchMap  -> InstrumentName
>                            -> (Channel, ProgNum)
> upmLookup upm iName = (chan, toGM iName)
>   where chan = maybe  (error (  "instrument " ++ show iName ++
>                                 " not in patch map")  )
>                       id (lookup iName upm)
> toMidi :: Performance -> UserPatchMap -> Midi
> toMidi pf upm =
>   let split     = splitByInst pf
>       insts     = map fst split
>       rightMap  =  if (allValid upm insts) then upm
>                    else (makeGMMap insts)
>   in Midi  (if length split == 1  then SingleTrack
>                                   else MultiTrack)
>            (TicksPerBeat division)
>            (map (fromAbsTime . performToMEvs rightMap) split)
>
> division = 96 :: Int
> allValid :: UserPatchMap -> [InstrumentName] -> Bool
> allValid upm = and . map (lookupB upm)
>
> lookupB :: UserPatchMap -> InstrumentName -> Bool
> lookupB upm x = or (map ((== x) . fst) upm)
> splitByInst :: Performance ->  [(InstrumentName,Performance)]
> splitByInst [] = []
> splitByInst pf = (i, pf1) : splitByInst pf2
>        where i          = eInst (head pf)
>              (pf1, pf2) = partition (\e -> eInst e == i) pf
> type MEvent = (Ticks, Message)
>
> defST = 500000
>
> performToMEvs ::  UserPatchMap
>                   -> (InstrumentName, Performance)
>                   -> [MEvent]
> performToMEvs upm (inm, pf) =
>   let  (chan,progNum)   = upmLookup upm inm
>        setupInst        = (0, ProgramChange chan progNum)
>        setTempo         = (0, TempoChange defST)
>        loop []      =  []
>        loop (e:es)  =  let (mev1,mev2) = mkMEvents chan e
>                        in mev1 : insertMEvent mev2 (loop es)
>   in setupInst : setTempo : loop pf
> mkMEvents :: Channel -> Event -> (MEvent,MEvent)
> mkMEvents  mChan (Event {  eTime = t, ePitch = p,
>                            eDur = d, eVol = v})
>                   = (  (toDelta t, NoteOn  mChan p v'),
>                        (toDelta (t+d), NoteOff mChan p v') )
>            where v' = max 0 (min 127 (fromIntegral v))
>
> toDelta t = round (t * 2.0 * fromIntegral division)
> insertMEvent :: MEvent -> [MEvent] -> [MEvent]
> insertMEvent mev1  []         = [mev1]
> insertMEvent mev1@(t1,_) mevs@(mev2@(t2,_):mevs') =
>       if t1 <= t2 then mev1 : mevs
>                   else mev2 : insertMEvent mev1 mevs'
>
> defUpm :: UserPatchMap
> defUpm = [(AcousticGrandPiano,1),
>           (Vibraphone,2),
>           (AcousticBass,3),
>           (Flute,4),
>           (TenorSax,5),
>           (AcousticGuitarSteel,6),
>           (Viola,7),
>           (StringEnsemble1,8),
>           (AcousticGrandPiano,9)]
>             -- the GM name for drums is unimportant, only channel 9
>
> testMidi :: Performable a => Music a -> Midi
> testMidi m = toMidi (defToPerf m) defUpm
>
> testMidiA :: Performable a => PMap Note1 -> Context Note1 -> Music a -> Midi
> testMidiA pm con m = toMidi (toPerf pm con m) defUpm
>
> test :: Performable a => Music a -> IO ()
> test     m = exportFile "test.mid" (testMidi m)
>
> testA :: Performable a => PMap Note1 -> Context Note1 -> Music a -> IO ()
> testA pm con m = exportFile "test.mid" (testMidiA pm con m)
>
> play :: Performable a => Music a -> IO ()
> play = playM . testMidi
>
> playM :: Midi -> IO ()
> playM midi = do
>   initialize
>   (defaultOutput playMidi) midi
>   terminate
>   return ()
>
> playA :: Performable a => PMap Note1 -> Context Note1
>          -> Music a -> IO ()
> playA pm con m =
>   let pf = fst $ perfDur pm con m
>   in playM (toMidi pf defUpm)
> makeMidi :: (Music1, Context Note1, UserPatchMap) -> Midi
> makeMidi (m,c,upm) = toMidi (perform defPMap c m) upm
>
> mToMF :: PMap a -> Context a -> UserPatchMap -> FilePath -> Music a -> IO ()
> mToMF pmap c upm fn m =
>       let pf = perform pmap c m
>           mf = toMidi pf upm
>       in exportFile fn mf
>
> gmUpm :: UserPatchMap
> gmUpm = map (\n -> (toEnum n, mod n 16 + 1)) [0..127]
>
> gmTest :: Int -> IO ()
> gmTest i =  let gMM = take 8 (drop (i*8) [0..127])
>                 mu  = line (map simple gMM)
>                 simple n = Modify (Instrument (toEnum n)) cMajArp
>             in  mToMF defPMap defCon gmUpm "test.mid" mu
>
> cMaj = [ n 4 qn | n <- [c,e,g] ]  -- octave 4, quarter notes
> cMajArp = toMusic1 (line cMaj)

BEGIN GENERALMIDI.HS DEFINITIONS


> fromGM :: Int -> InstrumentName
> fromGM i | i >= 0 && i <= 127 = toEnum i
> fromGM i = error $ "fromGMNo: " ++ show i ++
>                    " is not a valid General Midi Number"
> toGM :: InstrumentName -> Int
> toGM Percussion = 0
> toGM (Custom name) = 0
> toGM i = fromEnum i
>
> instance Enum InstrumentName where
>   fromEnum AcousticGrandPiano = 0
>   fromEnum BrightAcousticPiano = 1
>   fromEnum ElectricGrandPiano = 2
>   fromEnum HonkyTonkPiano = 3
>   fromEnum RhodesPiano = 4
>   fromEnum ChorusedPiano = 5
>   fromEnum Harpsichord = 6
>   fromEnum Clavinet = 7
>   fromEnum Celesta = 8
>   fromEnum Glockenspiel = 9
>   fromEnum MusicBox = 10
>   fromEnum Vibraphone = 11
>   fromEnum Marimba = 12
>   fromEnum Xylophone = 13
>   fromEnum TubularBells = 14
>   fromEnum Dulcimer = 15
>   fromEnum HammondOrgan = 16
>   fromEnum PercussiveOrgan = 17
>   fromEnum RockOrgan = 18
>   fromEnum ChurchOrgan = 19
>   fromEnum ReedOrgan = 20
>   fromEnum Accordion = 21
>   fromEnum Harmonica = 22
>   fromEnum TangoAccordion = 23
>   fromEnum AcousticGuitarNylon = 24
>   fromEnum AcousticGuitarSteel = 25
>   fromEnum ElectricGuitarJazz = 26
>   fromEnum ElectricGuitarClean = 27
>   fromEnum ElectricGuitarMuted = 28
>   fromEnum OverdrivenGuitar = 29
>   fromEnum DistortionGuitar = 30
>   fromEnum GuitarHarmonics = 31
>   fromEnum AcousticBass = 32
>   fromEnum ElectricBassFingered = 33
>   fromEnum ElectricBassPicked = 34
>   fromEnum FretlessBass = 35
>   fromEnum SlapBass1 = 36
>   fromEnum SlapBass2 = 37
>   fromEnum SynthBass1 = 38
>   fromEnum SynthBass2 = 39
>   fromEnum Violin = 40
>   fromEnum Viola = 41
>   fromEnum Cello = 42
>   fromEnum Contrabass = 43
>   fromEnum TremoloStrings = 44
>   fromEnum PizzicatoStrings = 45
>   fromEnum OrchestralHarp = 46
>   fromEnum Timpani = 47
>   fromEnum StringEnsemble1 = 48
>   fromEnum StringEnsemble2 = 49
>   fromEnum SynthStrings1 = 50
>   fromEnum SynthStrings2 = 51
>   fromEnum ChoirAahs = 52
>   fromEnum VoiceOohs = 53
>   fromEnum SynthVoice = 54
>   fromEnum OrchestraHit = 55
>   fromEnum Trumpet = 56
>   fromEnum Trombone = 57
>   fromEnum Tuba = 58
>   fromEnum MutedTrumpet = 59
>   fromEnum FrenchHorn = 60
>   fromEnum BrassSection = 61
>   fromEnum SynthBrass1 = 62
>   fromEnum SynthBrass2 = 63
>   fromEnum SopranoSax = 64
>   fromEnum AltoSax = 65
>   fromEnum TenorSax = 66
>   fromEnum BaritoneSax = 67
>   fromEnum Oboe = 68
>   fromEnum EnglishHorn = 69
>   fromEnum Bassoon = 70
>   fromEnum Clarinet = 71
>   fromEnum Piccolo = 72
>   fromEnum Flute = 73
>   fromEnum Recorder = 74
>   fromEnum PanFlute = 75
>   fromEnum BlownBottle = 76
>   fromEnum Shakuhachi = 77
>   fromEnum Whistle = 78
>   fromEnum Ocarina = 79
>   fromEnum Lead1Square = 80
>   fromEnum Lead2Sawtooth = 81
>   fromEnum Lead3Calliope = 82
>   fromEnum Lead4Chiff = 83
>   fromEnum Lead5Charang = 84
>   fromEnum Lead6Voice = 85
>   fromEnum Lead7Fifths = 86
>   fromEnum Lead8BassLead = 87
>   fromEnum Pad1NewAge = 88
>   fromEnum Pad2Warm = 89
>   fromEnum Pad3Polysynth = 90
>   fromEnum Pad4Choir = 91
>   fromEnum Pad5Bowed = 92
>   fromEnum Pad6Metallic = 93
>   fromEnum Pad7Halo = 94
>   fromEnum Pad8Sweep = 95
>   fromEnum FX1Train = 96
>   fromEnum FX2Soundtrack = 97
>   fromEnum FX3Crystal = 98
>   fromEnum FX4Atmosphere = 99
>   fromEnum FX5Brightness = 100
>   fromEnum FX6Goblins = 101
>   fromEnum FX7Echoes = 102
>   fromEnum FX8SciFi = 103
>   fromEnum Sitar = 104
>   fromEnum Banjo = 105
>   fromEnum Shamisen = 106
>   fromEnum Koto = 107
>   fromEnum Kalimba = 108
>   fromEnum Bagpipe = 109
>   fromEnum Fiddle = 110
>   fromEnum Shanai = 111
>   fromEnum TinkleBell = 112
>   fromEnum Agogo = 113
>   fromEnum SteelDrums = 114
>   fromEnum Woodblock = 115
>   fromEnum TaikoDrum = 116
>   fromEnum MelodicDrum = 117
>   fromEnum SynthDrum = 118
>   fromEnum ReverseCymbal = 119
>   fromEnum GuitarFretNoise = 120
>   fromEnum BreathNoise = 121
>   fromEnum Seashore = 122
>   fromEnum BirdTweet = 123
>   fromEnum TelephoneRing = 124
>   fromEnum Helicopter = 125
>   fromEnum Applause = 126
>   fromEnum Gunshot = 127
>   fromEnum i = error $ "fromEnum: " ++ show i ++ " inot implemented"
>
>   toEnum 0 = AcousticGrandPiano
>   toEnum 1 = BrightAcousticPiano
>   toEnum 2 = ElectricGrandPiano
>   toEnum 3 = HonkyTonkPiano
>   toEnum 4 = RhodesPiano
>   toEnum 5 = ChorusedPiano
>   toEnum 6 = Harpsichord
>   toEnum 7 = Clavinet
>   toEnum 8 = Celesta
>   toEnum 9 = Glockenspiel
>   toEnum 10 = MusicBox
>   toEnum 11 = Vibraphone
>   toEnum 12 = Marimba
>   toEnum 13 = Xylophone
>   toEnum 14 = TubularBells
>   toEnum 15 = Dulcimer
>   toEnum 16 = HammondOrgan
>   toEnum 17 = PercussiveOrgan
>   toEnum 18 = RockOrgan
>   toEnum 19 = ChurchOrgan
>   toEnum 20 = ReedOrgan
>   toEnum 21 = Accordion
>   toEnum 22 = Harmonica
>   toEnum 23 = TangoAccordion
>   toEnum 24 = AcousticGuitarNylon
>   toEnum 25 = AcousticGuitarSteel
>   toEnum 26 = ElectricGuitarJazz
>   toEnum 27 = ElectricGuitarClean
>   toEnum 28 = ElectricGuitarMuted
>   toEnum 29 = OverdrivenGuitar
>   toEnum 30 = DistortionGuitar
>   toEnum 31 = GuitarHarmonics
>   toEnum 32 = AcousticBass
>   toEnum 33 = ElectricBassFingered
>   toEnum 34 = ElectricBassPicked
>   toEnum 35 = FretlessBass
>   toEnum 36 = SlapBass1
>   toEnum 37 = SlapBass2
>   toEnum 38 = SynthBass1
>   toEnum 39 = SynthBass2
>   toEnum 40 = Violin
>   toEnum 41 = Viola
>   toEnum 42 = Cello
>   toEnum 43 = Contrabass
>   toEnum 44 = TremoloStrings
>   toEnum 45 = PizzicatoStrings
>   toEnum 46 = OrchestralHarp
>   toEnum 47 = Timpani
>   toEnum 48 = StringEnsemble1
>   toEnum 49 = StringEnsemble2
>   toEnum 50 = SynthStrings1
>   toEnum 51 = SynthStrings2
>   toEnum 52 = ChoirAahs
>   toEnum 53 = VoiceOohs
>   toEnum 54 = SynthVoice
>   toEnum 55 = OrchestraHit
>   toEnum 56 = Trumpet
>   toEnum 57 = Trombone
>   toEnum 58 = Tuba
>   toEnum 59 = MutedTrumpet
>   toEnum 60 = FrenchHorn
>   toEnum 61 = BrassSection
>   toEnum 62 = SynthBrass1
>   toEnum 63 = SynthBrass2
>   toEnum 64 = SopranoSax
>   toEnum 65 = AltoSax
>   toEnum 66 = TenorSax
>   toEnum 67 = BaritoneSax
>   toEnum 68 = Oboe
>   toEnum 69 = EnglishHorn
>   toEnum 70 = Bassoon
>   toEnum 71 = Clarinet
>   toEnum 72 = Piccolo
>   toEnum 73 = Flute
>   toEnum 74 = Recorder
>   toEnum 75 = PanFlute
>   toEnum 76 = BlownBottle
>   toEnum 77 = Shakuhachi
>   toEnum 78 = Whistle
>   toEnum 79 = Ocarina
>   toEnum 80 = Lead1Square
>   toEnum 81 = Lead2Sawtooth
>   toEnum 82 = Lead3Calliope
>   toEnum 83 = Lead4Chiff
>   toEnum 84 = Lead5Charang
>   toEnum 85 = Lead6Voice
>   toEnum 86 = Lead7Fifths
>   toEnum 87 = Lead8BassLead
>   toEnum 88 = Pad1NewAge
>   toEnum 89 = Pad2Warm
>   toEnum 90 = Pad3Polysynth
>   toEnum 91 = Pad4Choir
>   toEnum 92 = Pad5Bowed
>   toEnum 93 = Pad6Metallic
>   toEnum 94 = Pad7Halo
>   toEnum 95 = Pad8Sweep
>   toEnum 96 = FX1Train
>   toEnum 97 = FX2Soundtrack
>   toEnum 98 = FX3Crystal
>   toEnum 99 = FX4Atmosphere
>   toEnum 100 = FX5Brightness
>   toEnum 101 = FX6Goblins
>   toEnum 102 = FX7Echoes
>   toEnum 103 = FX8SciFi
>   toEnum 104 = Sitar
>   toEnum 105 = Banjo
>   toEnum 106 = Shamisen
>   toEnum 107 = Koto
>   toEnum 108 = Kalimba
>   toEnum 109 = Bagpipe
>   toEnum 110 = Fiddle
>   toEnum 111 = Shanai
>   toEnum 112 = TinkleBell
>   toEnum 113 = Agogo
>   toEnum 114 = SteelDrums
>   toEnum 115 = Woodblock
>   toEnum 116 = TaikoDrum
>   toEnum 117 = MelodicDrum
>   toEnum 118 = SynthDrum
>   toEnum 119 = ReverseCymbal
>   toEnum 120 = GuitarFretNoise
>   toEnum 121 = BreathNoise
>   toEnum 122 = Seashore
>   toEnum 123 = BirdTweet
>   toEnum 124 = TelephoneRing
>   toEnum 125 = Helicopter
>   toEnum 126 = Applause
>   toEnum 127 = Gunshot
>   toEnum n = error $ "toEnum: " ++ show n ++ " is not implemented for InstrumentName"

BEGIN MIDIIO.HS DEFINITIONS

----------------------------
 | Midi Type declarations |
----------------------------

> type MidiEvent = (Time, MidiMessage)

> data MidiMessage = ANote { channel :: !Channel, key :: !Key,
>                           velocity :: !Velocity, duration :: !Time }
>                  | Std Message
>   deriving Show


----------
 | Time |
----------

Is this the time we want?  This comes from PortMidi, but there's also the
function Euterpea.IO.MUI.SOE.timeGetTime which uses time data from GLFW.

> getTimeNow :: IO Time
> getTimeNow = do
>   t <- Sound.PortMidi.time -- XXX
>   return (fromIntegral t / 1000)


----------------------
 | Device Functions |
----------------------

getAllDevices returns a list of all of the DeviceInfos found.
It calls Port.Midi.getDeviceInfo over all device numbers

> getAllDevices :: IO [(DeviceID, DeviceInfo)]
> getAllDevices = do
>   n <- countDevices
>   deviceInfos <- mapM getDeviceInfo [0..n-1]
>   return $ zip [0..n-1] deviceInfos


isValidInputDevice and isValideOutputDevice check whether the given
devices are respectively valid for input or output.

> isValidInputDevice, isValidOutputDevice :: DeviceID -> IO Bool
> isValidInputDevice = isValidDevice input
> isValidOutputDevice = isValidDevice output
> isValidDevice :: (DeviceInfo -> Bool) -> DeviceID -> IO Bool
> isValidDevice pred i = do
>   n <- countDevices
>   info <- getAllDevices
>   return $
>     i >= 0 && i < n && pred (snd $ info !! i)


---------------------
 | Default devices |
---------------------

Rather than export the deviceIDs directly, these two functions allow
the caller to use the DeviceID without directly controlling it.

They take a function (such as playMidi) and an auxiary argument and
apply them together with the default device.  If no default device
exists, an error is thrown.

> defaultOutput :: (DeviceID -> a -> IO b) -> a -> IO b
> defaultOutput f a = do
>   i <- getDefaultOutputDeviceID
>   case i of
>     Nothing -> error "No MIDI output device found"
>     Just i  -> f i a
>
> defaultInput :: (DeviceID -> a -> IO b) -> a -> IO b
> defaultInput f a = do
>   i <- getDefaultInputDeviceID
>   case i of
>     Nothing -> error "No MIDI input device found"
>     Just i  -> f i a


-----------------------
 | Priority Channels |
-----------------------

The priority channel data type and a constructor for it will be used
by devices.  We define them here.

> data PrioChannel a b = PrioChannel
>     { get           :: IO (Heap.MinPrioHeap a b),
>       push          :: a -> b -> IO (),
>       pop           :: IO (a,b),
>       peek          :: IO (Maybe (a,b)) }

> makePriorityChannel :: IO (PrioChannel Time Message)
> makePriorityChannel = do
>   heapRef <- newIORef (Heap.empty :: Heap.MinPrioHeap Time Message)
>   let get = readIORef heapRef
>       push a b = modifyIORef heapRef (Heap.insert (a,b))
>       pop = do
>         h <- get
>         let (a, h') = Heap.extractHead h
>         modifyIORef heapRef (\_ -> h')
>         return a
>       peek = do
>         h <- get
>         if Heap.isEmpty h
>           then return Nothing
>           else return $ Just $ Heap.head h
>
>   return $ PrioChannel get push pop peek


------------------------
 | Global Device Data |
------------------------

We keep a mapping from DeviceID to the priority channel for keeping
track of future MIDI messages, an output function to produce sound,
and a stop function.  This mapping is stored in the global ref
outDevMap, and it is accessed by getOutDev (which looks up info
and adds associations if necessary) and terminateMidi (which calls
the stop function on all elements and clears the mapping).

outDevMap is the global mapping.

> outDevMap :: IORef [(DeviceID,
>                      (PrioChannel Time Message, -- priority channel
>                       (Time, Message) -> IO (), -- sound output function
>                       IO ()))]                  -- stop/terminate function
> outDevMap = unsafePerformIO $ newIORef []


outPort and inPort are global memory refs that contain a mapping of
DeviceID to Port Midi Streams.  They are modified with addPort (which
adds a new mapping to the list) and lookupPort (which, given a DeviceID,
returns the Port Midi Stream associated with it).

> outPort, inPort :: IORef [(DeviceID, PMStream)]
> outPort = unsafePerformIO (newIORef [])
> inPort  = unsafePerformIO (newIORef [])

> lookupPort :: IORef [(DeviceID, PMStream)] -> DeviceID -> IO (Maybe PMStream)
> lookupPort p i = readIORef p >>= (return . lookup i)

> addPort :: IORef [(DeviceID, PMStream)] -> (DeviceID, PMStream) -> IO ()
> addPort p is = modifyIORef p (is:)


--------------------------------------------------
 | Global Device Initialization and Termination |
--------------------------------------------------

initializeMidi just initializes PortMidi

> initializeMidi :: IO ()
> initializeMidi = do
>   e <- initialize
>   if e == NoError
>       then return ()
>       else reportError "initializeMidi" e

terminateMidi calls the stop function on all elements of outDevMap
and clears the mapping entirely.  It also clears outPort and inPort.

> terminateMidi :: IO ()
> terminateMidi = do
>   inits <- readIORef outDevMap
>   mapM_ (\(_, (_,_out,stop)) -> stop) inits
>   terminate
>   modifyIORef outDevMap (const [])
>   writeIORef outPort []
>   writeIORef inPort []


-------------------
 | Device Lookup |
-------------------

getOutDev looks up info in outDevMap and adds associations if necessary.
It is accessed as a helper function for outputMidi and deliverMidiEvent.

> getOutDev :: DeviceID -> IO (PrioChannel Time Message, (Time, Message) -> IO (), IO ())
> getOutDev devId = do
>   inits <- readIORef outDevMap
>   case lookup devId inits of
>     Just f -> return f
>     Nothing -> do
>         x <- midiOutRealTime' devId -- Changes made by Donya Quick: this line used to pattern match against Just.
>         pChan <- makePriorityChannel
>         case x of Just (mout,stop) -> do -- Case statement added.
>                                         modifyIORef outDevMap ((devId,(pChan,mout,stop)):)
>                                         return (pChan,mout,stop)
>                   Nothing -> return (pChan, const (return ()), return ()) -- Nothing case added


----------------
 | Midi Input |
----------------

pollMidi take an input device and a callback function and polls the device
for midi events.  Any events are sent, along with the current time, to
the callback function.
DWC NOTE: Why is the time even used?  All messages get the same time?
          Also, should the callback function perhaps take [Message]?

> pollMidi :: DeviceID -> ((Time, Message) -> IO ()) -> IO ()
> pollMidi devId callback = do
>   s <- lookupPort inPort devId
>   case s of
>     Nothing -> do
>       r <- openInput devId
>       case r of
>         Right e -> reportError "pollMIDI" e
>         Left s -> addPort inPort (devId, s) >> input s
>     Just s -> input s
>   where
>     input :: PMStream -> IO ()
>     input s = do
>       e <- readEvents s
>       case e of
>         Right e -> if e == NoError
>           then return ()
>           else reportError "pollMIDI" e
>         Left l -> do
>           t <- getTimeNow
>           sendEvts t l
>       where
>         sendEvts _now [] = return ()
>         sendEvts now  ((PMEvent m t):l) =
>           case msgToMidi m of
>             Just m' -> callback (now, m') >> sendEvts now l
>             Nothing -> sendEvts now l


---------------------------------------------
 | Midi Output for inidividual Midi events |
---------------------------------------------

The following two functions are for sending and playing individual
Midi events to devices.  Typically, usage will be to call outputMidi
to play anything that's ready to play and then send in the latest
messages with deliverMidiEvent.  Of course, if no new messages are
ready to be delivered, that step can be omitted.  Either way,
outputMidi should be called many times per second to assure that
all Midi messages are played approximately when scheduled.

deliverMidiEvent sends the given MidiEvent to the given device.  If
the event is scheduled to happen ``now'', then it is immediately
played.  Otherwise, it is queued for later.

> deliverMidiEvent :: DeviceID -> MidiEvent -> IO ()
> deliverMidiEvent devId (t,m) = do
>   (pChan, out, _stop) <- getOutDev devId
>   now <- getTimeNow
>   let deliver t m = do
>       if t == 0
>         then out (now,m)
>         else push pChan (now+t) m
>
>   case m of
>     Std m -> deliver t m
>     ANote c k v d -> do
>         deliver t     (NoteOn c k v)
>         deliver (t+d) (NoteOff c k v)


outputMidi plays all midi events that are waiting in this device's
priority queue whose time to play has come.

> outputMidi :: DeviceID -> IO ()
> outputMidi devId = do
>   (pChan, out, _stop) <- getOutDev devId
>   let loop = do
>         r <- peek pChan
>         case r of
>           Nothing     -> return ()
>           Just (t,m)  -> do
>             now <- getTimeNow
>             if t <= now
>               then out (now, m) >> pop pChan >> loop
>               else return ()
>   loop
>   return ()


-------------------------------------------
 | Midi Output for a complete Midi track |
-------------------------------------------

When an entire Midi track is ready to be played, the playMidi function
may be more appropriate than deliverMidiEvent and outputMidi.

playMidi will queue up the entire Midi track given to it and then close
the output device.

> playMidi :: DeviceID -> Midi -> IO ()
> playMidi device midi@(Midi _ division _) = do
>   let track = toRealTime division (toAbsTime (head (tracks (toSingleTrack midi))))
>   out <- midiOutRealTime device
>   case out of
>     Nothing -> return ()
>     Just (out, stop) -> do
>       t0 <- getTimeNow
>       finally (playTrack t0 0 out track) stop
>   where
>     playTrack t0 t' out [] = out (t0 + t', TrackEnd)
>     playTrack t0 t' out (e@(t, m) : s) = do
>       out (t0 + t, m)
>       if isTrackEnd m
>         then return ()
>         else playTrack t0 t out s


---------------------
 | midiOutRealTime |
---------------------

The following two functions are used to open a device for Midi output.
They should only be called when the device hasn't yet been opened, and
they both return a ``play'' function and a ``stop'' function.

Currently, midiOutRealTime' is used for Midi output for inidividual
Midi events, and midiOutRealTime is used for Midi output for a complete
Midi track.

DWC Notes:
I'm not entirely sure how they both work yet.  midiOutRealTime'
actually looks pretty straightforward in that it just creates the process
and stop functions and adds this device to the outPort device list.  The
process function will look up the device in the outPort device list, and
if it finds it, it writes the message to it.  The stop function removes
the device from the outPort list and closes it.

On the other hand, midiOutRealTime spawns a new thread and does some
concurrent stuff.  Really, it looks similar, but I don't know when to
use one and when to use the other.

> midiOutRealTime' :: DeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime' i = do
>   s <- openOutput i 1
>   case s of
>     Right e -> reportError "Unable to open output device in midiOutRealTime'" e >> return Nothing
>     Left s -> do
>       addPort outPort (i, s)
>       return $ Just (process i, finalize i)
>   where
>     process i (t, msg) = do
>       s <- lookupPort outPort i
>       case s of
>         Nothing -> error ("midiOutRealTime': port " ++ show i ++ " is not open for output")
>         Just s -> do
>           if isTrackEnd msg
>               then return ()
>               else case midiEvent msg of
>                 Just m  -> writeMsg s t m
>                 Nothing -> return ()
>     writeMsg s t m = do
>               e <- writeShort s (PMEvent m (round (t * 1e3)))
>               case e of
>                 NoError -> return ()
>                 _ -> reportError "midiOutRealTime'" e
>     finalize i = do
>       s <- lookupPort outPort i
>       e <- maybe (return NoError) close s
>       case e of
>         NoError -> return ()
>         _ -> reportError "midiOutRealTime'" e


> midiOutRealTime :: DeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
> midiOutRealTime i = do
>   s <- openOutput i 1
>   case s of
>     Right e -> reportError "outputMidi" e >> return Nothing
>     Left s -> do
>       ch <- atomically newTChan
>       wait <- newEmptyMVar
>       fin <- newEmptyMVar
>       forkIO (pump s ch wait fin)
>       return $ Just (output s ch wait, stop ch fin)
>   where
>     stop ch fin = atomically (unGetTChan ch Nothing) >> takeMVar fin
>     output s ch wait evt@(_, m) = do
>       atomically $ writeTChan ch (Just evt)
>       if isTrackEnd m then takeMVar wait else return ()
>     pump s ch wait fin = loop
>       where
>         loop = do
>           e <- atomically $ readTChan ch
>           case e of
>             Nothing -> close s >> putMVar fin ()
>             Just (t, msg) -> do
>               now <- getTimeNow
>               if (t > now + 5)
>                 then atomically (unGetTChan ch e) >> threadDelay 10000 >> loop
>                 else do
>                   done <- process t msg
>                   if done
>                     then waitUntil (t + 1)
>                     else loop
>           where
>             waitUntil t = do
>               now <- getTimeNow
>               if t > now
>                 then do
>                   threadDelay $ min 10000 (round((t - now) * 1E6))
>                   empty <- atomically $ isEmptyTChan ch
>                   if empty
>                     then waitUntil t
>                     else do
>                       e <- atomically $ readTChan ch
>                       case e of
>                         Nothing -> finishup
>                         _ -> waitUntil t
>                 else finishup
>             finishup = putMVar wait () >> close s >> putMVar fin ()
>             process t msg = if isTrackEnd msg
>               then return True
>               else case midiEvent msg of
>                 Just m  -> writeMsg t m
>                 Nothing -> return False
>             writeMsg t m = do
>               e <- writeShort s (PMEvent m (round (t * 1e3)))
>               case e of
>                 NoError -> return False
>                 BufferOverflow -> putStrLn "overflow" >> threadDelay 10000 >> writeMsg t m
>                 _ -> reportError "outputMidi" e >> return True


---------------------
 | MIDI Conversion |
---------------------

A conversion function from Codec.Midi Messages to PortMidi PMMsgs.

> midiEvent :: Message -> Maybe PMMsg
> midiEvent (NoteOff c p v)         = Just $ PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (NoteOn c p v)          = Just $ PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
> midiEvent (KeyPressure c p pr)    = Just $ PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
> midiEvent (ControlChange c cn cv) = Just $ PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
> midiEvent (ProgramChange c pn)    = Just $ PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
> midiEvent (ChannelPressure c pr)  = Just $ PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
> midiEvent (PitchWheel c pb)       = Just $ PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
>  where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)
> midiEvent _ = Nothing


A conversion function from PortMidi PMMsgs to Codec.Midi Messages.

> msgToMidi :: PMMsg -> Maybe Message
> msgToMidi (PMMsg m d1 d2) =
>   let k = (m .&. 0xF0) `shiftR` 4
>       c = fromIntegral (m .&. 0x0F)
>   in case k of
>     0x8 -> Just $ NoteOff c (fromIntegral d1) (fromIntegral d2)
>     0x9 -> Just $ NoteOn  c (fromIntegral d1) (fromIntegral d2)
>     0xA -> Just $ KeyPressure c (fromIntegral d1) (fromIntegral d2)
>     0xB -> Just $ ControlChange c (fromIntegral d1) (fromIntegral d2)
>     0xC -> Just $ ProgramChange c (fromIntegral d1)
>     0xD -> Just $ ChannelPressure c (fromIntegral d1)
>     0xE -> Just $ PitchWheel c (fromIntegral (d1 + d2 `shiftL` 8))
>     0xF -> Nothing -- SysEx event not handled
>     _   -> Nothing


---------------------
 | Error Reporting |
---------------------

> reportError :: String -> PMError -> IO ()
> reportError prompt e = do
>   err <- getErrorText e
>   hPutStrLn stderr $ prompt ++ ": " ++  err





----------------------
 | Unused Functions |
----------------------

> -- Prints all DeviceInfo found by getAllDevices.
> printAllDeviceInfo :: IO ()
> printAllDeviceInfo = do
>   devs <- getAllDevices
>   mapM_ (print . snd) devs

> -- Given whether the device is an input device and the device name,
> -- returns the DeviceID.
> getDeviceId :: Bool -> String -> IO (Maybe DeviceID)
> getDeviceId isInput n = do
>   devs <- getAllDevices
>   return $ findIndex (\(_,d) -> name d == n && input d == isInput) devs

> playTrackRealTime :: DeviceID -> [(t, Message)] -> IO ()
> playTrackRealTime device track = do
>   out <- midiOutRealTime device
>   case out of
>     Nothing -> return ()
>     Just (out, stop) -> finally (playTrack out track) stop
>   where
>     playTrack out [] = do
>       t <- getTimeNow
>       out (t, TrackEnd)
>     playTrack out (e@(_, m) : s) = do
>       t <- getTimeNow
>       out (t, m)
>       if isTrackEnd m
>         then return ()
>         else playTrack out s

> {-
>     ticksPerBeat = case division of
>       TicksPerBeat n -> n
>       TicksPerSecond mode nticks -> (256 - mode - 128) * nticks `div` 2
> -}

> {-
> runTrack tpb = runTrack' 0 0 120                 -- 120 beat/s is the default tempo
>   where
>     runTrack' t t0 bps ((_, TempoChange tempo) : l) =
>       let bps' = 1000000 `div` fromIntegral tempo
>       in runTrack' t t0 bps' l
>     runTrack' t t0 bps ((t1, m) : l) =
>       let t' = t + 1000 * fromIntegral (t1 - t0) `div` (tpb * bps)
>       in (t', m) : runTrack' t' t1 bps l
>     runTrack' _ _ _ [] = []

> playTrack s ch t0 = playTrack' 0
>   where
>     playTrack' t [] = putStrLn "done" >> putMVar ch Nothing >> return (round (t * 1.0E3))
>     playTrack' _ ((t, e):es) = putMVar ch (Just io) >> playTrack' t es
>       where
>         io = case midiEvent e of
>           Just m  -> writeShort s (PMEvent m (t0 + round (t * 1.0E3)))
>           Nothing -> return NoError
> -}

> recordMidi :: DeviceID -> (Track Time -> IO ()) -> IO ()
> recordMidi device f = do
>   ch <- newChan
>   final <- midiInRealTime device (\e -> writeChan ch e >> return False)
>   case final of
>     Nothing  -> return ()
>     Just fin -> do
>       track <- getChanContents ch
>       done <- newEmptyMVar
>       forkIO (f track >> putMVar done ())
>       putStrLn "Start recording, hit ENTER when you are done."
>       getLine
>       fin
>       takeMVar done
>       return ()

> midiInRealTime :: DeviceID -> ((Time, Message) -> IO Bool) -> IO (Maybe (IO ()))
> midiInRealTime device callback = do
>   r <- openInput device
>   case r of
>     Right e -> reportError "midiInRealTime" e >> return Nothing
>     Left s -> do
>       fin <- newEmptyMVar
>       forkIO (loop Nothing s fin)
>       return (Just (putMVar fin () >> putMVar fin ()))
>   where
>     loop start s fin = do
>       done <- tryTakeMVar fin
>       t <- getTimeNow
>       case done of
>         Just _ -> close s >> callback (t, TrackEnd) >> takeMVar fin >> return ()
>         Nothing -> do
>           e <- readEvents s
>           case e of
>             Right e -> if e == NoError
>               then threadDelay 1000 >> loop start s fin
>               else do
>                 reportError "midiInRealTime" e
>                 callback (t, TrackEnd)
>                 return ()
>             Left l -> do
>               t <- getTimeNow
>               sendEvts start t l
>       where
>         sendEvts start now [] = loop start s fin
>         sendEvts start now (e@(PMEvent m t):l) = do
>           let t0 = maybe t id start
>           case msgToMidi m of
>             Just m' -> do
>               done <- callback (now + fromIntegral (t - t0) / 1E3, m')
>               if done then close s >> return () else sendEvts (Just t0) now l
>             Nothing -> sendEvts (Just t0) now l

