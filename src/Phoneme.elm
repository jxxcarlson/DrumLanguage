module Phoneme exposing
    ( PhonemeClass(..)
    , Pitch(..)
    , PitchClass(..)
    , multiplyRests
    , phonemeClassOfString
    , pitchOfPhonemeClass1
    , stringOfPhonemeClass
    , stringOfPitch
    )


type PhonemeClass
    = Vowel
    | Plosive
    | VoicedPlosive
    | Fricative
    | VoicedFricative
    | Nasal
    | Approximant
    | X
    | Silence
    | Punctuation
    | Unknown


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


pitchOfPhonemeClass1 : PhonemeClass -> Pitch
pitchOfPhonemeClass1 pc =
    case pc of
        Vowel ->
            Pitch C 3

        Nasal ->
            Pitch Eb 3

        VoicedFricative ->
            Pitch G 3

        Fricative ->
            Pitch Bb 3

        VoicedPlosive ->
            Pitch D 4

        Plosive ->
            Pitch F 4

        Approximant ->
            Pitch Ab 4

        Silence ->
            Rest

        Punctuation ->
            Pitch C 2

        _ ->
            Pitch G 1


stringOfPhonemeClass : PhonemeClass -> String
stringOfPhonemeClass pc =
    case pc of
        Vowel ->
            "v"

        Plosive ->
            "p"

        VoicedPlosive ->
            "vp"

        Fricative ->
            "f"

        VoicedFricative ->
            "vf"

        Nasal ->
            "n"

        Approximant ->
            "a"

        X ->
            "x"

        Silence ->
            "s"

        Punctuation ->
            "||"

        Unknown ->
            "U"



--
--
--


vowels =
    [ "a", "e", "i", "o", "u", "y" ]


plosives =
    [ "p", "t", "k", "c", "q" ]


voicedPlosives =
    [ "b", "d", "g" ]


fricatives =
    [ "f", "h", "s" ]


voicedFricatives =
    [ "v", "z" ]


nasals =
    [ "m", "n" ]


approximants =
    [ "r", "l", "j", "w" ]


xs =
    [ "x" ]


punctuationSymbols =
    [ ".", ",", "!", "?" ]


phonemeClassOfString : String -> PhonemeClass
phonemeClassOfString str =
    if List.member str vowels then
        Vowel

    else if List.member str plosives then
        Plosive

    else if List.member str voicedPlosives then
        VoicedPlosive

    else if List.member str fricatives then
        Fricative

    else if List.member str voicedFricatives then
        VoicedFricative

    else if List.member str nasals then
        Nasal

    else if List.member str approximants then
        Approximant

    else if List.member str xs then
        X

    else if List.member str punctuationSymbols then
        Punctuation

    else if str == " " then
        Silence

    else
        Unknown


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
