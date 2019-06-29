module Pitch exposing
    ( Pitch(..)
    , PitchClass(..)
    , multiplyRests
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


type Pitch
    = Pitch PitchClass Int
    | Rest


stringOfPitch : Pitch -> String
stringOfPitch pitch =
    case pitch of
        Pitch pitchClass octave ->
            stringOfPitchClass pitchClass ++ String.fromInt octave

        Rest ->
            "Rest"


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


multiplyRests : Int -> List Pitch -> List Pitch
multiplyRests k pitchList =
    pitchList
        |> List.map
            (\x ->
                if x == Rest then
                    List.repeat k Rest

                else
                    [ x ]
            )
        |> List.concat
