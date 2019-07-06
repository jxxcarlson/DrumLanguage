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
import Json.Encode as E
import Phoneme
    exposing
        ( PhonemeClass(..)
        , phonemeClassOfString
        , pitchOfPhonemeClass1
        , stringOfPhonemeClass
        )
import Pitch
    exposing
        ( Pitch(..)
        , PitchClass(..)
        , multiplyRests
        , stringOfPitch
        )


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
    , bpmString : String
    }



--
-- MSG
--


type Msg
    = NoOp
    | InputText String
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


port sendNotes : List String -> Cmd msg


port sendCommand : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , output = ""
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
            ( { model | input = initialText }, Cmd.none )

        Sample1 ->
            ( { model | input = sample1Text }, Cmd.none )

        Sample2 ->
            ( { model | input = sample2Text }, Cmd.none )

        InputText str ->
            ( { model | input = str }, Cmd.none )

        InputBPM str ->
            ( { model | bpmString = str }, Cmd.none )

        SetTempo ->
            ( model, sendCommand <| "tempo:" ++ model.bpmString )

        Play ->
            let
                pitchOfPhonemeClass =
                    pitchOfPhonemeClass2

                noteList =
                    model.input
                        |> String.toLower
                        |> String.split ""
                        |> List.map phonemeClassOfString
                        |> List.filter (\s -> s /= Unknown)
                        |> List.map pitchOfPhonemeClass
                        |> List.map stringOfPitch
            in
            ( { model
                | output = noteList |> List.take 30 |> String.join " "
              }
            , Cmd.batch [ sendCommand <| "tempo:" ++ model.bpmString, sendNotes noteList ]
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
            , inputText model
            , outputDisplay model
            , appButtons model
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.size 24 ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX, Font.size 11 ]
        [ text model.output ]


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ width (px 700), height (px 200) ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [] (text "")
        , spellcheck = False
        }


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


pitchOfPhonemeClass3 : PhonemeClass -> Pitch
pitchOfPhonemeClass3 pc =
    case pc of
        Vowel ->
            Pitch C 3

        Nasal ->
            Pitch E 3

        VoicedFricative ->
            Pitch F 3

        Fricative ->
            Pitch G 3

        VoicedPlosive ->
            Pitch Bb 3

        Plosive ->
            Pitch C 4

        Approximant ->
            Pitch D 4

        Silence ->
            Rest

        Punctuation ->
            Pitch C 2

        _ ->
            Pitch G 4


pitchOfPhonemeClass2 : PhonemeClass -> Pitch
pitchOfPhonemeClass2 pc =
    case pc of
        Vowel ->
            Pitch G 2

        Approximant ->
            Pitch C 3

        Nasal ->
            Pitch E 3

        VoicedFricative ->
            Pitch F 3

        Fricative ->
            Pitch G 3

        VoicedPlosive ->
            Pitch Bb 3

        Plosive ->
            Pitch D 4

        Silence ->
            Rest

        Punctuation ->
            Pitch C 2

        _ ->
            Pitch G 1
