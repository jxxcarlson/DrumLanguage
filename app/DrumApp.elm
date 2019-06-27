module DrumApp exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Drum exposing (SoundId(..))
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http


soundDict =
    Drum.setDict
        [ ( " ", SoundId 0 )
        , ( "a", SoundId 1 )
        , ( "b", SoundId 2 )
        , ( "c", SoundId 3 )
        , ( "d", SoundId 4 )
        , ( "e", SoundId 5 )
        , ( "f", SoundId 6 )
        , ( "g", SoundId 7 )
        , ( "h", SoundId 8 )
        , ( "i", SoundId 9 )
        , ( "j", SoundId 11 )
        , ( "k", SoundId 12 )
        , ( "l", SoundId 13 )
        , ( "m", SoundId 14 )
        , ( "n", SoundId 15 )
        , ( "o", SoundId 16 )
        , ( "p", SoundId 17 )
        , ( "q", SoundId 18 )
        , ( "r", SoundId 19 )
        , ( "s", SoundId 20 )
        , ( "t", SoundId 21 )
        , ( "u", SoundId 22 )
        , ( "v", SoundId 23 )
        , ( "w", SoundId 24 )
        , ( "x", SoundId 25 )
        , ( "y", SoundId 26 )
        , ( "z", SoundId 27 )
        , ( ".", SoundId 28 )
        , ( ",", SoundId 29 )
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    }


type Msg
    = NoOp
    | InputText String
    | Play


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "Write something here."
      , output = ""
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str, output = str }, Cmd.none )

        Play ->
            ( { model
                | output =
                    model.input
                        |> String.toLower
                        |> Drum.soundIdsWithDict soundDict
                        |> Drum.stringFromSoundIdList
              }
            , Cmd.none
            )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "Drum app"
            , inputText model
            , outputDisplay model
            , appButton
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


inputText : Model -> Element Msg
inputText model =
    Input.text [ width (px 600) ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [] (text "")
        }


appButton : Element Msg
appButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just Play
            , label = el [ centerX, centerY ] (text "Play")
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , width (px 700)
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]
