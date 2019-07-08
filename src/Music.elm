module Music exposing
    ( Control
    , Music(..)
    , duration
    , note
    , rest
    , sequence
    , stack
    )

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


sequence : List (Music a) -> Music a
sequence list =
    Sequence list


stack : List (Music a) -> Music a
stack list =
    Stack list


duration : Music a -> Rational
duration music =
    case music of
        Prim primitive ->
            Primitive.duration primitive

        Sequence list ->
            Rational.sum (List.map duration list)

        Stack list ->
            Rational.maxList (List.map duration list)


{-|

> note en (C, 3)
> Prim (Note (R 1 8) (C,3))

-}
note : Duration -> a -> Music a
note dur a =
    Prim (Note dur a)


rest : Duration -> Music a
rest dur =
    Prim (Rest dur)
