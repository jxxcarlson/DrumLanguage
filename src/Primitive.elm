module Primitive exposing (Primitive(..), duration, stringOfPrimitive, tranposeOctavePrimitivePitch)

import Duration exposing (..)
import Pitch exposing (Pitch, stringOfPitch)
import Player exposing (..)
import Rational exposing (Rational)


type Primitive a
    = Note Duration a
    | Rest Duration


duration : Primitive a -> Rational
duration p =
    case p of
        Note dur a ->
            dur

        Rest dur ->
            dur


stringOfPrimitive : Primitive Pitch -> String
stringOfPrimitive primitive =
    case primitive of
        Note dur pitch ->
            stringOfPitch pitch

        Rest dur ->
            "Rest"


tranposeOctavePrimitivePitch : Int -> Primitive Pitch -> Primitive Pitch
tranposeOctavePrimitivePitch octave primitive =
    case primitive of
        Note dur pitch ->
            Note dur (Pitch.transposeOctave octave pitch)

        Rest dur ->
            Rest dur
