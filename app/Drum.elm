port module Drum exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Encode as Encode
import Melody
import Player


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { voice1String : String
    , voice2String : String
    , notesForVoice1 : String
    , notesForVoice2 : String
    , bpmString : String
    }



--
-- MSG
--


type Msg
    = NoOp
    | ReadVoice1 String
    | ReadVoice2 String
    | InputBPM String
    | Play
    | Stop
      --| Tempo Int
    | SetTempo
    | Instructions
    | Sample1
    | Sample2


type alias Flags =
    {}


port sendPiece : Encode.Value -> Cmd msg


port sendCommand : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { voice1String = initialText
      , voice2String = ""
      , notesForVoice1 = ""
      , notesForVoice2 = ""
      , bpmString = "320"
      }
    , Cmd.none
    )


initialText =
    String.replace "\n"
        " "
        """This app turns text into a kind of
techno-music by imitating the princples of African
drum languages.  Things to try: (1) put text in
this box and press "Play".  (2) Alter the tempo
(beats per minute). (3) Try patterns, e.g.,
"Wawachaachaadadadada,,"  Here spaces and commas both give
one beat rests. (4) It is fun to experiment
with patterns like palindromes:
What is si tahW is si sii sii,,,,
or just like this: dtaattddttaa,
"""



-- Wawachaachaadadadada,,tawwat isiwa


sample1Text =
    "What is si tahW is si sii sii,,,, or just like this: dtaattddttaa,"


sample2Text =
    "MississipiipississiM,Wawachaachaadadadada,,"


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Instructions ->
            ( { model | voice1String = initialText }, Cmd.none )

        Sample1 ->
            ( { model | voice1String = sample1Text }, Cmd.none )

        Sample2 ->
            ( { model | voice1String = sample2Text }, Cmd.none )

        ReadVoice1 str ->
            ( { model | voice1String = str }, Cmd.none )

        ReadVoice2 str ->
            ( { model | voice2String = str }, Cmd.none )

        InputBPM str ->
            ( { model | bpmString = str }, Cmd.none )

        SetTempo ->
            ( model, sendCommand <| "tempo:" ++ model.bpmString )

        Play ->
            let
                noteList1 =
                    Melody.fromString model.voice1String

                noteList2 =
                    Melody.fromString model.voice2String

                part1 =
                    Player.partFromMelody "4n" "0.9" noteList1

                part2 =
                    Player.partFromMelody "4n" "0.9" noteList2

                piece =
                    { bpm = String.toInt model.bpmString |> Maybe.withDefault 72
                    , parts = [ part1, part2 ]
                    }
            in
            ( { model
                | notesForVoice1 = noteList1 |> List.take 30 |> String.join " "
                , notesForVoice2 = noteList2 |> List.take 30 |> String.join " "
              }
            , Cmd.batch
                [ sendCommand <| "tempo:" ++ model.bpmString
                , sendPiece <| Player.encodePiece piece
                ]
            )

        Stop ->
            ( model, sendCommand "stop:now" )



--
-- Tempo bpm ->
--     ( model, sendCommand <| "tempo:" ++ String.fromInt bpm )
--
--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 40 40 40) ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "Techno Drum Language App"
            , readVoice1 model
            , readVoice2 model
            , appButtons model
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.size 24 ] [ text str ]


displayVoice : String -> Element msg
displayVoice notes =
    row [ centerX, Font.size 11 ]
        [ text notes ]


readVoice1 : Model -> Element Msg
readVoice1 model =
    column [ spacing 8 ]
        [ el [ Font.bold, Font.size 14 ] (text <| "Voice 1")
        , Input.multiline [ width (px 700), height (px 200) ]
            { onChange = ReadVoice1
            , text = model.voice1String
            , placeholder = Nothing
            , label = Input.labelLeft [] <| el [] (text "")
            , spellcheck = False
            }
        , displayVoice model.notesForVoice1
        ]


readVoice2 : Model -> Element Msg
readVoice2 model =
    column [ spacing 8 ]
        [ el [ Font.bold, Font.size 14 ] (text <| "Voice 2")
        , Input.multiline [ width (px 700), height (px 200) ]
            { onChange = ReadVoice2
            , text = model.voice2String
            , placeholder = Nothing
            , label = Input.labelLeft [] <| el [] (text "")
            , spellcheck = False
            }
        , displayVoice model.notesForVoice2
        ]


inputBPM : Model -> Element Msg
inputBPM model =
    Input.text [ width (px 60) ]
        { onChange = InputBPM
        , text = model.bpmString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ moveDown 13, paddingXY 4 0 ] (text "BPM")
        }


appButtons : Model -> Element Msg
appButtons model =
    row [ centerX, spacing 20 ]
        [ instructionsButton
        , sampleButton1
        , sampleButton2
        , Input.button buttonStyle
            { onPress = Just Play
            , label = el [ centerX, centerY ] (text "Play")
            }
        , Input.button buttonStyle
            { onPress = Just Stop
            , label = el [ centerX, centerY ] (text "Stop")
            }
        , inputBPM model
        ]


instructionsButton =
    Input.button buttonStyle
        { onPress = Just Instructions
        , label = el [ centerX, centerY ] (text "Instructions")
        }


sampleButton1 =
    Input.button buttonStyle
        { onPress = Just Sample1
        , label = el [ centerX, centerY ] (text "Sample 1")
        }


sampleButton2 =
    Input.button buttonStyle
        { onPress = Just Sample2
        , label = el [ centerX, centerY ] (text "Sample 2")
        }


tempoButton =
    Input.button buttonStyle
        { onPress = Just SetTempo
        , label = el [ centerX, centerY ] (text "Set tempo")
        }



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , width (px 800)
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , Font.size 16
    , paddingXY 15 8
    ]
