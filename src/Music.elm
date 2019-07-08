module Music exposing (Control, Music(..), duration)

import Duration exposing (..)
import Html exposing (a)
import Primitive exposing (..)
import Rational exposing (Rational, add, max, sum)


type Music a
    = Prim (Primitive a)
    | Sequence (List (Music a))
    | Stack (List (Music a))



-- | Modify Control (Music a)


type Control
    = Tempo Rational


duration : Music a -> Rational
duration music =
    case music of
        Prim primitive ->
            Primitive.duration primitive

        Sequence list ->
            Rational.sum (List.map duration list)

        Stack list ->
            Rational.maxList (List.map duration list)
