module Color exposing
    ( appBGColor
    , buttonBGColor
    , buttonText
    , gray
    , inputText
    , red
    , text
    , windowBGColor
    )

import Element exposing (rgb255)


inputText =
    gray 40


text =
    gray 250


buttonText =
    gray 200


red =
    rgb255 130 0 0


gray g =
    rgb255 g g g


windowBGColor =
    gray 110


appBGColor =
    gray 70


buttonBGColor =
    gray 10
