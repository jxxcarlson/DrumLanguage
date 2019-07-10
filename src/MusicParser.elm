module MusicParser exposing (noteParser)

import Duration exposing (Duration)
import Parser exposing (..)
import Pitch exposing (Pitch, PitchClass(..))
import Primitive exposing (Primitive(..))
import Rational exposing (Rational(..))


type Noot
    = N Pitch Duration


noteParser : Parser (Primitive Pitch)
noteParser =
    (succeed N
        |= pitchParser
        |. spaces
        |= durationParser
    )
        |> map (\(N p d) -> Note d p)


{-|

> run pitchParser "c 8"
> Ok (C,8) : Result (List Parser.DeadEnd) Pitch.Pitch

-}
pitchParser : Parser Pitch
pitchParser =
    succeed (\pc n -> ( pc, n ))
        |= pitchClassParser
        |= int


pitchClassParser : Parser PitchClass
pitchClassParser =
    (getChompedString <|
        succeed ()
            |. chompIf (\c -> List.member c [ 'c', 'd', 'e', 'f', 'g', 'a', 'b' ])
            |. oneOf [ symbol " ", symbol "f ", symbol "ff ", symbol "s ", symbol "ss " ]
    )
        |> map String.trim
        |> map pitchClassOfString


durationParser : Parser Duration
durationParser =
    (getChompedString <|
        succeed ()
            |. oneOf [ symbol "bn", symbol "ddhn", symbol "ddqn", symbol "dden", symbol "den", symbol "dhn", symbol "dqn", symbol "dsn", symbol "dtn", symbol "dwn", symbol "en", symbol "hn", symbol "qn", symbol "sfn", symbol "sn", symbol "wn" ]
    )
        |> map durationOfString


pitchClassOfString : String -> PitchClass
pitchClassOfString str =
    case str of
        "cff" ->
            Cff

        "cf" ->
            Cf

        "c" ->
            C

        "dff" ->
            Dff

        "cs" ->
            Cs

        "df" ->
            Df

        "css" ->
            Css

        "d" ->
            D

        "eff" ->
            Eff

        "ds" ->
            Ds

        "ef" ->
            Ef

        "fff" ->
            Fff

        "dss" ->
            Dss

        "e" ->
            E

        "ff" ->
            Ff

        "es" ->
            Es

        "f" ->
            F

        "gff" ->
            Gff

        "ess" ->
            Ess

        "fs" ->
            Fs

        "gf" ->
            Gf

        "fss" ->
            Fss

        "g" ->
            G

        "aff" ->
            Aff

        "gs" ->
            Gs

        "af" ->
            Af

        "gss" ->
            Gss

        "a" ->
            A

        "bff" ->
            Bff

        "as" ->
            As

        "bf" ->
            Bf

        "ass" ->
            Ass

        "b" ->
            B

        "bs" ->
            Bs

        "bss" ->
            Bss

        _ ->
            Bss


durationOfString : String -> Duration
durationOfString str =
    case str of
        "bn" ->
            R 2 1

        "ddhn" ->
            R 7 8

        "ddqn" ->
            R 7 16

        "dden" ->
            R 7 32

        "dwn" ->
            R 3 2

        "dhn" ->
            R 3 4

        "dqn" ->
            R 3 8

        "den" ->
            R 3 16

        "dsn" ->
            R 3 32

        "dtn" ->
            R 3 64

        "wn" ->
            R 1 1

        "hn" ->
            R 1 2

        "qn" ->
            R 1 4

        "en" ->
            R 1 8

        "sn" ->
            R 1 16

        "tn" ->
            R 1 32

        "sfn" ->
            R 1 64

        _ ->
            R 1 64



--- |= oneOf [ symbol "cff", symbol "cf", symbol "c", symbol "dff", symbol "cs", symbol "df", symbol "css", symbol "d", symbol "eff", symbol "ds", symbol "ef", symbol "fff", symbol "dss", symbol "e", symbol "ff", symbol "es", symbol "f", symbol "gff", symbol "ess", symbol "fs", symbol "gf", symbol "fss", symbol "g", symbol "aff", symbol "gs", symbol "af", symbol "gss", symbol "a", symbol "bff", symbol "as", symbol "bf", symbol "ass", symbol "b", symbol "bs", symbol "bss" ]
