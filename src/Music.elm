module Music exposing (Control, Music(..), duration)

import Duration exposing (..)
import Html exposing (a)
import Primitive exposing (..)
import Rational exposing (Rational, add, max, sum)


type Music a
    = Prim (Primitive a)
    | L (List (Music a))
    | Sequence (Music a) (Music a)
    | Stack (Music a) (Music a)



-- | Modify Control (Music a)


type Control
    = Tempo Rational


duration : Music a -> Rational
duration music =
    case music of
        Prim primitive ->
            Primitive.duration primitive

        L list ->
            Rational.sum (List.map duration list)

        Sequence p q ->
            Rational.add (duration p) (duration q)

        Stack p q ->
            Rational.max (duration p) (duration q)
