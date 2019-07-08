module Music exposing
    ( Control
    , Music(..)
    , duration
    , note
    , rest
    , sequence
    , stack
    )

import Duration exposing (..)
import Html exposing (a, var)
import Pitch exposing (..)
import Primitive exposing (..)
import Rational exposing (Rational, add, max, sum)


type Music a
    = Prim (Primitive a)
    | Sequence (List (Music a))
    | Stack (List (Music a))
    | Modify Control (Music a)


{-| Incomplete. See HSM p. 32
-}
type Control
    = Tempo Rational
    | Transpose AbsPitch
    | Phrase (List PhraseAttribute)
    | Instrument InstrumentName
    | KeySig PitchClass Mode
    | Custom String


type Mode
    = Major
    | Minor
    | Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
    | CustomMode String


type PhraseAttribute
    = Dyn Dynamic
    | Tmp Tempo
    | Art Articulation
    | Orn Ornament


type Dynamic
    = Accent Rational
    | Crescendo Rational
    | Diminuendo
    | Rational
    | StdLoudness StdLoudness
    | Loudness Rational


type StdLoudness
    = PPP
    | PP
    | P
    | MP
    | SF
    | MF
    | NF
    | FF
    | FFF


type Tempo
    = Ritardando Rational
    | Accelerando Rational


type Articulation
    = Staccato Rational
    | Legato Rational
    | Slurred Rational
    | Tenuto
    | Marcato
    | Pedal
    | Fermata
    | FermataDown
    | Breath
    | DownBow
    | UpBow
    | Harmonic
    | Pizzicato
    | LeftPizz
    | BartokPizz
    | Swell
    | Wedge
    | Thumb
    | Stopped


type Ornament
    = Trill
    | Mordent
    | InvMordent
    | DoubleMordent
    | Turn
    | TrilledTurn
    | ShortTrill
    | Arpeggio
    | ArpeggioUp
    | ArpeggioDown
    | Instruction String
    | Head NoteHead
    | DiatonicTrans Int


type NoteHead
    = SquareHead
    | XHead
    | TriangleHead
    | TremoloHead
    | SlashHead
    | ArtHarmonic
    | NoHead


type InstrumentName
    = AccousticGrandPiano
    | BrightAcousticGrandPiano
    | ElectricGrandPiano
    | HonkyTonkPiano
    | RhodesPiano
    | ChorusedPiano
    | Harpsichord
    | Clavinet
    | Celesta
    | Glockenspiel
    | MusicBox
    | Vibraphone
    | Marimba
    | Xylophone
    | TubularBells
    | Dulcimer
    | HammondOrgan
    | PercussiveOrgan
    | RockOrgan
    | ChurchOrgan
    | ReedOrgan
    | Accordion
    | Harmonica
    | TangoAccordion
    | AcousticGuitarNylon
    | AcousticGuitarSteel
    | ElectricGuitarJazz
    | ElectricGuitarClean
    | ElectricGuitarMuted
    | OverdrivenGuitar
    | DistortionGuitar
    | GuitarHarmonics
    | AcousticBass
    | ElectricBassFingered
    | ElectricBassPicked
    | FretlessBass
    | SlapBass1
    | SlapBass2
    | SynthBass1
    | SynthBass2
    | Violin
    | Viola
    | Cello
    | Contrabass
    | TremoloStrings
    | PizzicatoStrings
    | OrchestralHarp
    | Timpani
    | StringEnsemble1
    | StringEnsemble2
    | SynthStrings1
    | SynthStrings2
    | ChoirAahs
    | VoiceOohs
    | SynthVoice
    | OrchestraHit
    | Trumpet
    | Trombone
    | Tuba
    | MutedTrumpet
    | FrenchHorn
    | BrassSection
    | SynthBrass1
    | SynthBrass2
    | SopranoSax
    | AltoSax
    | TenorSax
    | BaritoneSax
    | Oboe
    | Bassoon
    | EnglishHorn
    | Clarinet
    | Piccolo
    | Flute
    | Recorder
    | PanFlute
    | BlownBottle
    | Shakuhachi
    | Whistle
    | Ocarina
    | Lead1Square
    | Lead2Sawtooth
    | Lead3Calliope
    | Lead4Chiff
    | Lead5Charang
    | Lead6Voice
    | Lead7Fifths
    | Lead8BassLead
    | Pad1NewAge
    | Pad2Warm
    | Pad3Polysynth
    | Pad4Choir
    | Pad5Bowed
    | Pad6Metallic
    | Pad7Halo
    | Pad8Sweep
    | FX1Train
    | FX2Soundtrack
    | FX3Crystal
    | FX4Atmosphere
    | FX5Brightness
    | FX6Goblins
    | FX7Echoes
    | FX8SciFi
    | Sitar
    | Banjo
    | Shamisen
    | Koto
    | Kalimba
    | Bagpipe
    | Fiddle
    | Shanai
    | TinkleBell
    | Agogo
    | SteelDrums
    | Woodblock
    | TaikoDrum
    | MelodicDrum
    | SynthDrum
    | ReverseCymbal
    | GuitarFretNoise
    | BreathNoise
    | Seashore
    | BirdTweet
    | TelephoneRing
    | Helicopter
    | Applause
    | Gunshot
    | Percussion
    | CustomInstrument String


sequence : List (Music a) -> Music a
sequence list =
    Sequence list


stack : List (Music a) -> Music a
stack list =
    Stack list


duration : Music a -> Rational
duration music =
    case music of
        Prim primitive ->
            Primitive.duration primitive

        Sequence list ->
            Rational.sum (List.map duration list)

        Stack list ->
            Rational.maxList (List.map duration list)

        Modify _ music_ ->
            duration music_


{-|

> note en (C, 3)
> Prim (Note (R 1 8) (C,3))

-}
note : Duration -> x -> Music x
note dur x =
    Prim (Note dur x)


rest : Duration -> Music a
rest dur =
    Prim (Rest dur)


cff o d_ =
    note d_ ( Cff, o )


cf o d_ =
    note d_ ( Cf, o )


c o d_ =
    note d_ ( C, o )


cs o d_ =
    note d_ ( Cs, o )


css o d_ =
    note d_ ( Css, o )



--


dff o d_ =
    note d_ ( Dff, o )


df o d_ =
    note d_ ( Df, o )


d o d_ =
    note d_ ( D, o )


ds o d_ =
    note d_ ( Ds, o )


dss o d_ =
    note d_ ( Dss, o )



--


eff o d_ =
    note d_ ( Eff, o )


ef o d_ =
    note d_ ( Ef, o )


e o d_ =
    note d_ ( E, o )


es o d_ =
    note d_ ( Es, o )


ess o d_ =
    note d_ ( Ess, o )



--
--


fff o d_ =
    note d_ ( Fff, o )


ff o d_ =
    note d_ ( Ff, o )


f o d_ =
    note d_ ( F, o )


fs o d_ =
    note d_ ( Fs, o )


fss o d_ =
    note d_ ( Fss, o )



--


gff o d_ =
    note d_ ( Gff, o )


gf o d_ =
    note d_ ( Gf, o )


g o d_ =
    note d_ ( G, o )


gs o d_ =
    note d_ ( Gs, o )


gss o d_ =
    note d_ ( Gss, o )



--


aff o d_ =
    note d_ ( Aff, o )


af o d_ =
    note d_ ( Af, o )


a o d_ =
    note d_ ( A, o )


aS o d_ =
    note d_ ( As, o )


ass o d_ =
    note d_ ( Ass, o )



--


bff o d_ =
    note d_ ( Bff, o )


bf o d_ =
    note d_ ( Bf, o )


b o d_ =
    note d_ ( B, o )


bs o d_ =
    note d_ ( Bs, o )


bss o d_ =
    note d_ ( Bss, o )
