module Phoneme exposing
    ( PhonemeClass(..)
    , phonemeClassOfString
    , primitiveOfPhonemeClass1
    , stringOfPhonemeClass
    , toPitchNameList
    )

import Duration exposing (..)
import Pitch exposing (Pitch, PitchClass(..))
import Primitive exposing (..)
import Rationals exposing (..)


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


toPitchNameList : String -> List String
toPitchNameList str =
    str
        |> String.toLower
        |> String.split ""
        |> List.map phonemeClassOfString
        |> List.filter (\s -> s /= Unknown)
        |> List.map primitiveOfPhonemeClass1
        |> List.map stringOfPrimitive


pitchOfPhonemeClass : PhonemeClass -> Primitive ()
pitchOfPhonemeClass pc =
    case pc of
        Vowel ->
            P ( G, 2 ) qn ()

        Approximant ->
            P ( C, 3 ) qn ()

        Nasal ->
            P ( E, 3 ) qn ()

        VoicedFricative ->
            P ( F, 3 ) qn ()

        Fricative ->
            P ( G, 3 ) qn ()

        VoicedPlosive ->
            P ( Bb, 3 ) qn ()

        Plosive ->
            P ( D, 4 ) qn ()

        Silence ->
            Rest qn

        Punctuation ->
            P ( C, 2 ) qn ()

        _ ->
            P ( G, 1 ) qn ()


primitiveOfPhonemeClass1 : PhonemeClass -> Primitive ()
primitiveOfPhonemeClass1 pc =
    case pc of
        Vowel ->
            P ( C, 3 ) qn ()

        Nasal ->
            P ( Eb, 3 ) qn ()

        VoicedFricative ->
            P ( G, 3 ) qn ()

        Fricative ->
            P ( Bb, 3 ) qn ()

        VoicedPlosive ->
            P ( D, 4 ) qn ()

        Plosive ->
            P ( F, 4 ) qn ()

        Approximant ->
            P ( Ab, 4 ) qn ()

        Silence ->
            Rest qn

        Punctuation ->
            P ( C, 2 ) qn ()

        _ ->
            P ( G, 1 ) qn ()


primitiveOfPhonemeClass1a : PhonemeClass -> Primitive ()
primitiveOfPhonemeClass1a pc =
    case pc of
        Vowel ->
            P ( G, 1 ) qn ()

        Nasal ->
            P ( E, 3 ) qn ()

        VoicedFricative ->
            P ( G, 3 ) qn ()

        Fricative ->
            P ( Bb, 3 ) qn ()

        VoicedPlosive ->
            P ( D, 4 ) qn ()

        Plosive ->
            P ( Eb, 4 ) qn ()

        Approximant ->
            P ( Db, 3 ) qn ()

        Silence ->
            Rest qn

        Punctuation ->
            P ( C, 2 ) qn ()

        _ ->
            P ( G, 1 ) qn ()


pitchOfPhonemeClass3 : PhonemeClass -> Primitive ()
pitchOfPhonemeClass3 pc =
    case pc of
        Vowel ->
            P ( C, 3 ) qn ()

        Nasal ->
            P ( E, 3 ) qn ()

        VoicedFricative ->
            P ( F, 3 ) qn ()

        Fricative ->
            P ( G, 3 ) qn ()

        VoicedPlosive ->
            P ( Bb, 3 ) qn ()

        Plosive ->
            P ( C, 4 ) qn ()

        Approximant ->
            P ( D, 4 ) qn ()

        Silence ->
            Rest qn

        Punctuation ->
            P ( C, 2 ) qn ()

        _ ->
            P ( G, 4 ) qn ()


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
    [ ".", "!", "?" ]


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

    else if str == " " || str == "," then
        Silence

    else
        Unknown
