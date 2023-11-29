module Pitch exposing
    ( AbsPitch
    , Pitch
    , PitchClass(..)
    , stringOfPitch
    , tjsStringOfPitch
    , transposeOctave
    )


type PitchClass
    = Cff
    | Cf
    | C
    | Dff
    | Cs
    | Df
    | Css
    | D
    | Eff
    | Ds
    | Ef
    | Fff
    | Dss
    | E
    | Ff
    | Es
    | F
    | Gff
    | Ess
    | Fs
    | Gf
    | Fss
    | G
    | Aff
    | Gs
    | Af
    | Gss
    | A
    | Bff
    | As
    | Bf
    | Ass
    | B
    | Bs
    | Bss


type alias Octave =
    Int


type alias Pitch =
    ( PitchClass, Octave )


transposeOctave : Int -> Pitch -> Pitch
transposeOctave k ( pitchclass, octave ) =
    ( pitchclass, octave + k )


type alias AbsPitch =
    Int


stringOfPitch : Pitch -> String
stringOfPitch ( pitchClass, octave ) =
    stringOfPitchClass pitchClass ++ String.fromInt octave


tjsStringOfPitch : Pitch -> String
tjsStringOfPitch ( pitchClass, octave ) =
    tjsStringOfPitchClass pitchClass ++ String.fromInt octave


stringOfPitchClass : PitchClass -> String
stringOfPitchClass pitchClass =
    case pitchClass of
        Cff ->
            "Cff"

        Cf ->
            "Cf"

        C ->
            "C"

        Dff ->
            "Dff"

        Cs ->
            "C#"

        Df ->
            "Df"

        Css ->
            "Css"

        D ->
            "D"

        Eff ->
            "Eff"

        Ds ->
            "Ds"

        Ef ->
            "Ef"

        Fff ->
            "Fff"

        Dss ->
            "Dss"

        E ->
            "E"

        Ff ->
            "Ff"

        Es ->
            "Es"

        F ->
            "F"

        Gff ->
            "Gff"

        Ess ->
            "Ess"

        Fs ->
            "Fs"

        Gf ->
            "Gf"

        Fss ->
            "Fss"

        G ->
            "G"

        Aff ->
            "Aff"

        Gs ->
            "Gs"

        Af ->
            "Af"

        Gss ->
            "Gss"

        A ->
            "A"

        Bff ->
            "Bff"

        As ->
            "A#"

        Bf ->
            "Bf"

        Ass ->
            "Ass"

        B ->
            "B"

        Bs ->
            "Bs"

        Bss ->
            "Bss"


tjsStringOfPitchClass : PitchClass -> String
tjsStringOfPitchClass pitchClass =
    case pitchClass of
        Cff ->
            "Bb"

        Cf ->
            "Cb"

        C ->
            "C"

        Dff ->
            "C"

        Cs ->
            "C#"

        Df ->
            "Db"

        Css ->
            "D"

        D ->
            "D"

        Eff ->
            "D"

        Ds ->
            "D#"

        Ef ->
            "Eb"

        Fff ->
            "Eb"

        Dss ->
            "E"

        E ->
            "E"

        Ff ->
            "Fb"

        Es ->
            "E#"

        F ->
            "F"

        Gff ->
            "F"

        Ess ->
            "F#"

        Fs ->
            "F#"

        Gf ->
            "Gb"

        Fss ->
            "G"

        G ->
            "G"

        Aff ->
            "G"

        Gs ->
            "G#"

        Af ->
            "Ab"

        Gss ->
            "A"

        A ->
            "A"

        Bff ->
            "A"

        As ->
            "A#"

        Bf ->
            "Bb"

        Ass ->
            "B"

        B ->
            "B"

        Bs ->
            "C"

        Bss ->
            "C#"
