module Music exposing (Music(..))

import Duration exposing (..)
import Primitive exposing (..)


type Music a
    = Prim (Primitive a)
    | Sequence (Music a) (Music a)
    | Stack (Music a) (Music a)
