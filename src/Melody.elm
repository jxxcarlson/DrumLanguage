module Melody exposing (fromString)

import Phoneme
    exposing
        ( PhonemeClass(..)
        , phonemeClassOfString
        , pitchOfPhonemeClass1
        , stringOfPhonemeClass
        )
import Pitch
    exposing
        ( Pitch(..)
        , PitchClass(..)
        , multiplyRests
        , stringOfPitch
        )


fromString : String -> List String
fromString str =
    str
        |> String.toLower
        |> String.split ""
        |> List.map phonemeClassOfString
        |> List.filter (\s -> s /= Unknown)
        |> List.map pitchOfPhonemeClass
        |> List.map stringOfPitch


pitchOfPhonemeClass : PhonemeClass -> Pitch
pitchOfPhonemeClass pc =
    case pc of
        Vowel ->
            Pitch G 2

        Approximant ->
            Pitch C 3

        Nasal ->
            Pitch E 3

        VoicedFricative ->
            Pitch F 3

        Fricative ->
            Pitch G 3

        VoicedPlosive ->
            Pitch Bb 3

        Plosive ->
            Pitch D 4

        Silence ->
            Rest

        Punctuation ->
            Pitch C 2

        _ ->
            Pitch G 1


pitchOfPhonemeClass3 : PhonemeClass -> Pitch
pitchOfPhonemeClass3 pc =
    case pc of
        Vowel ->
            Pitch C 3

        Nasal ->
            Pitch E 3

        VoicedFricative ->
            Pitch F 3

        Fricative ->
            Pitch G 3

        VoicedPlosive ->
            Pitch Bb 3

        Plosive ->
            Pitch C 4

        Approximant ->
            Pitch D 4

        Silence ->
            Rest

        Punctuation ->
            Pitch C 2

        _ ->
            Pitch G 4
