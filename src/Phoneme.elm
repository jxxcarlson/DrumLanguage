module Phoneme exposing
    ( PhonemeClass(..)
    , phonemeClassOfString
    , primitiveOfPhonemeClass1
    , stringOfPhonemeClass
    , toPitchNameList
    , toPitchNameListTransposeOctave
    )

import Duration exposing (..)
import Pitch exposing (Pitch, PitchClass(..))
import Primitive exposing (..)
import Rational exposing (..)


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
        |> List.map primitiveOfPhonemeClass3
        |> List.map stringOfPrimitive


toPitchNameListTransposeOctave : Int -> String -> List String
toPitchNameListTransposeOctave octaveDelta str =
    str
        |> String.toLower
        |> String.split ""
        |> List.map phonemeClassOfString
        |> List.filter (\s -> s /= Unknown)
        |> List.map primitiveOfPhonemeClass3
        |> List.map (\p -> Primitive.tranposeOctavePrimitivePitch octaveDelta p)
        |> List.map stringOfPrimitive


pitchOfPhonemeClass : PhonemeClass -> Primitive Pitch
pitchOfPhonemeClass pc =
    case pc of
        Vowel ->
            Note qn ( G, 2 )

        Approximant ->
            Note qn ( C, 3 )

        Nasal ->
            Note qn ( E, 3 )

        VoicedFricative ->
            Note qn ( F, 3 )

        Fricative ->
            Note qn ( G, 3 )

        VoicedPlosive ->
            Note qn ( Bf, 3 )

        Plosive ->
            Note qn ( D, 4 )

        Silence ->
            Rest qn

        Punctuation ->
            Note qn ( C, 2 )

        _ ->
            Note qn ( G, 1 )


primitiveOfPhonemeClass1 : PhonemeClass -> Primitive Pitch
primitiveOfPhonemeClass1 pc =
    case pc of
        Vowel ->
            Note qn ( C, 3 )

        Nasal ->
            Note qn ( Ef, 3 )

        VoicedFricative ->
            Note qn ( G, 3 )

        Fricative ->
            Note qn ( Bf, 3 )

        VoicedPlosive ->
            Note qn ( D, 4 )

        Plosive ->
            Note qn ( F, 4 )

        Approximant ->
            Note qn ( Af, 4 )

        Silence ->
            Rest qn

        Punctuation ->
            Note qn ( C, 2 )

        _ ->
            Note qn ( G, 1 )


primitiveOfPhonemeClass1a : PhonemeClass -> Primitive Pitch
primitiveOfPhonemeClass1a pc =
    case pc of
        Vowel ->
            Note qn ( G, 1 )

        Nasal ->
            Note qn ( E, 3 )

        VoicedFricative ->
            Note qn ( G, 3 )

        Fricative ->
            Note qn ( Bf, 3 )

        VoicedPlosive ->
            Note qn ( D, 4 )

        Plosive ->
            Note qn ( Ef, 4 )

        Approximant ->
            Note qn ( Df, 3 )

        Silence ->
            Rest qn

        Punctuation ->
            Note qn ( C, 2 )

        _ ->
            Note qn ( G, 1 )


primitiveOfPhonemeClass3 : PhonemeClass -> Primitive Pitch
primitiveOfPhonemeClass3 pc =
    case pc of
        Vowel ->
            Note qn ( C, 3 )

        Nasal ->
            Note qn ( E, 3 )

        VoicedFricative ->
            Note qn ( F, 3 )

        Fricative ->
            Note qn ( G, 3 )

        VoicedPlosive ->
            Note qn ( Bf, 3 )

        Plosive ->
            Note qn ( C, 4 )

        Approximant ->
            Note qn ( D, 4 )

        Silence ->
            Rest qn

        Punctuation ->
            Note qn ( C, 2 )

        _ ->
            Note qn ( G, 4 )


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
