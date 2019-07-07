module Pitch exposing
    ( Pitch
    , PitchClass(..)
    , stringOfPitch
    )


type PitchClass
    = C
    | Cs
    | Db
    | D
    | Ds
    | Eb
    | E
    | F
    | Fs
    | Gb
    | G
    | Gs
    | Ab
    | A
    | As
    | Bb
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

        Db ->
            "Db"

        D ->
            "D"

        Ds ->
            "D#"

        Eb ->
            "Eb"

        E ->
            "E"

        F ->
            "F"

        Fs ->
            "F#"

        Gb ->
            "Gb"

        G ->
            "G"

        Gs ->
            "Gs"

        Ab ->
            "Ab"

        A ->
            "A"

        As ->
            "A#"

        Bb ->
            "Bb"

        B ->
            "B"
