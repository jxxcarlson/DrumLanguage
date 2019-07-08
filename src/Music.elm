module Music exposing (Music(..))

import Duration exposing (..)
import Html exposing (a)
import Primitive exposing (..)


type Music a
    = Prim (Primitive a)
    | L (List (Music a))
    | Sequence (Music a) (Music a)
    | Stack (Music a) (Music a)
