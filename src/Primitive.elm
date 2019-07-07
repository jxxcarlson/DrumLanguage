module Primitive exposing (Primitive(..), stringOfPrimitive)

import Duration exposing (..)
import Pitch exposing (Pitch, stringOfPitch)
import Player exposing (..)


type Primitive a
    = P Pitch Duration a
    | Rest Duration


stringOfPrimitive : Primitive a -> String
stringOfPrimitive primitive =
    case primitive of
        P pitch duration a ->
            stringOfPitch pitch

        Rest duration ->
            "Rest"
