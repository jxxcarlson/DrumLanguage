module Player exposing (Part, Piece, encodePiece, partFromMelody)

import Json.Encode as Encode


encodePart : Part -> Encode.Value
encodePart part =
    Encode.object
        [ ( "melody", Encode.list Encode.string part.melody )
        , ( "rhythm", Encode.list Encode.string part.rhythm )
        , ( "velocity", Encode.list Encode.string part.velocity )
        ]


encodePiece : Piece -> Encode.Value
encodePiece piece =
    Encode.list encodePart piece


partFromMelody : String -> String -> List String -> Part
partFromMelody beatNote velocity melody =
    let
        beats =
            List.length melody
    in
    { melody = melody
    , rhythm = List.repeat beats beatNote
    , velocity = List.repeat beats velocity
    }


type alias Part =
    { melody : List String
    , rhythm : List String
    , velocity : List String
    }


type alias Piece =
    List Part
