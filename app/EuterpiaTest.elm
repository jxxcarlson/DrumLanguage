port module Example exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Duration exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Encode as Encode
import Maybe.Extra
import Music exposing (..)
import MusicParser
import Parser
import Phoneme
import Pitch exposing (Pitch)
import Rational
import Songs
import ToneJSPlayer


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
    , voice1Music : Maybe (Music Pitch)
    , voice2Music : Maybe (Music Pitch)
    , bpmString : String
    }


v1Init =
    "c 3 qn, e 3 qn, g 3 qn, d 4 hn, c 4 wn"


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { voice1String = v1Init
      , voice2String = ""
      , voice1Music = MusicParser.parseSequence v1Init |> Result.toMaybe
      , voice2Music = Nothing
      , bpmString = "80"
      }
    , Cmd.none
    )


dMinor =
    stack [ d 4 wn, f 4 wn, a 4 wn ]


gMajor =
    Music.stack [ g 4 wn, b 4 wn, d 4 wn ]


cMajor =
    stack [ c 4 bn, ee 4 bn, g 4 bn ]


iiVI =
    sequence [ dMinor, gMajor, cMajor ]


arp1 =
    sequence [ d 2 wn, f 2 wn, a 2 wn, d 3 hn, f 3 hn, a 3 hn, d 5 qn, f 5 qn, a 5 qn, d 6 en, f 6 en, a 6 en, d 3 wn ]


eventList =
    ToneJSPlayer.eventListOfMusic 60 iiVI



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
    | Sample1
    | Sample2


type alias Flags =
    {}


port sendMusic : Encode.Value -> Cmd msg


port sendCommand : String -> Cmd msg



-- Wawachaachaadadadada,,tawwat isiwa


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Sample1 ->
            ( { model
                | voice1String = Songs.s1
                , voice1Music = MusicParser.parseSequence Songs.s1 |> Result.toMaybe
              }
            , Cmd.none
            )

        Sample2 ->
            ( { model
                | voice1String = Songs.s2
                , voice1Music = MusicParser.parseSequence Songs.s2 |> Result.toMaybe
              }
            , Cmd.none
            )

        ReadVoice1 str ->
            ( { model | voice1String = str, voice1Music = MusicParser.parseSequence str |> Result.toMaybe }, Cmd.none )

        ReadVoice2 str ->
            ( { model | voice2String = str, voice2Music = MusicParser.parseSequence str |> Result.toMaybe }, Cmd.none )

        InputBPM str ->
            ( { model | bpmString = str }, Cmd.none )

        SetTempo ->
            ( model, sendCommand <| "tempo:" ++ model.bpmString )

        Play ->
            let
                parts =
                    Maybe.Extra.values [ model.voice1Music, model.voice2Music ]

                sendMusicCmd =
                    sendMusic <| ToneJSPlayer.encodeParts <| List.map (ToneJSPlayer.eventListOfMusic (bpm model)) parts

                --
                -- case ( model.voice1Music, model.voice2Music ) of
                --     ( Just music1, Just music2 ) ->
                --         sendMusic <| preparePiece music1 music2
                --
                --     ( Just music1, Nothing ) ->
                --         sendMusic <| ToneJSPlayer.encodeEventList <| ToneJSPlayer.eventListOfMusic (bpm model) <| music1
                --
                --     ( Nothing, Just music2 ) ->
                --         sendMusic <| ToneJSPlayer.encodeEventList <| ToneJSPlayer.eventListOfMusic (bpm model) <| music2
                --
                --     ( Nothing, Nothing ) ->
                --         Cmd.none
            in
            ( model
            , Cmd.batch
                [ sendMusicCmd ]
            )

        Stop ->
            ( model
            , Cmd.batch
                [ sendCommand "stop:now" ]
            )



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
            [ title "Euterpia Test"
            , readVoice1 model
            , readVoice2 model
            , appButtons model
            , newTabLink [ centerX, Font.size 12 ]
                { url = "https://jxxcarlson.io/posts/2019-06-29-drum-language/"
                , label = el [ Font.size 14, Font.color <| Element.rgb 0 0 1 ] (text "Article")
                }
            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.size 24 ] [ text str ]


displayVoice : Maybe (Music Pitch) -> Element msg
displayVoice music =
    let
        message =
            case Maybe.map Music.duration music of
                Nothing ->
                    "Parser error"

                Just dur ->
                    "Duration: " ++ Rational.stringValue dur
    in
    row [ centerX, Font.size 11 ]
        [ text <| message ]


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
        , displayVoice model.voice1Music
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
        , displayVoice model.voice2Music
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
        [ sampleButton1
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



--
-- HELPERS
--


bpm : Model -> Int
bpm model =
    String.toInt model.bpmString |> Maybe.withDefault 120
