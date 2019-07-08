module Pitch exposing
    ( Pitch
    , PitchClass(..)
    , stringOfPitch
    )


type PitchClass
    = C
    | Cs
    | Df
    | D
    | Ds
    | Ef
    | E
    | F
    | Fs
    | Gf
    | G
    | Gs
    | Af
    | A
    | As
    | Bf
    | B


type alias Octave =
    Int


type alias Pitch =
    ( PitchClass, Octave )


stringOfPitch : Pitch -> String
stringOfPitch ( pitchClass, octave ) =
    stringOfPitchClass pitchClass ++ String.fromInt octave


stringOfPitchClass : PitchClass -> String
stringOfPitchClass pitchClass =
    case pitchClass of
        C ->
            "C"

        Cs ->
            "C#"

        Df ->
            "Df"

        D ->
            "D"

        Ds ->
            "D#"

        Ef ->
            "Ef"

        E ->
            "E"

        F ->
            "F"

        Fs ->
            "F#"

        Gf ->
            "Gf"

        G ->
            "G"

        Gs ->
            "Gs"

        Af ->
            "Af"

        A ->
            "A"

        As ->
            "A#"

        Bf ->
            "Bf"

        B ->
            "B"
