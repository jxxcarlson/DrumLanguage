port module Drum exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Color
import DrumSongs
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Phoneme
import Player
import Rational
import Time


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
    , bpm : Float
    , beats : Int
    , clockRunning : Bool
    , activeSample : ActiveSample
    , voices : List Voice
    , appState : AppState
    }


type AppState
    = Playing
    | Stopped
    | Reset


type ActiveSample
    = ActiveSample1
    | ActiveSample2
    | ActiveSampleNone


type Voice
    = Voice1
    | Voice2



--
-- MSG
--


type Msg
    = NoOp
    | Beat Time.Posix
    | ReadVoice1 String
    | ReadVoice2 String
    | InputBPM String
    | Play
    | Stop
    | DoReset
      --| Tempo Int
    | SetTempo
    | Instructions
    | Sample1
    | Sample2
    | MuteVoice1
    | MuteVoice2


type alias Flags =
    {}


port sendPiece : Encode.Value -> Cmd msg


port sendCommand : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { voice1String = DrumSongs.initialTextVoice1
      , voice2String = DrumSongs.initialTextVoice2
      , notesForVoice1 = ""
      , notesForVoice2 = ""
      , bpmString = "200"
      , bpm = 200
      , beats = 0
      , clockRunning = False
      , activeSample = ActiveSampleNone
      , voices = [ Voice1, Voice2 ]
      , appState = Stopped
      }
    , Cmd.none
    )



-- Wawachaachaadadadada,,tawwat isiwa


subscriptions model =
    Time.every (1000 * 60 / model.bpm) Beat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Beat _ ->
            ( { model
                | beats =
                    if model.clockRunning then
                        model.beats + 1

                    else
                        model.beats
              }
            , Cmd.none
            )

        Instructions ->
            ( { model
                | voice1String = DrumSongs.initialTextVoice1
                , voice2String = DrumSongs.initialTextVoice2
                , appState = Stopped
                , clockRunning = False
                , beats = 0
                , activeSample = ActiveSampleNone
              }
            , sendCommand "stop:now"
            )

        Sample1 ->
            ( { model
                | activeSample = ActiveSample1
                , voice1String = DrumSongs.sample1TextVoice1
                , voice2String = DrumSongs.sample1TextVoice2
                , appState = Stopped
                , clockRunning = False
                , beats = 0
              }
            , sendCommand "stop:now"
            )

        Sample2 ->
            ( { model
                | activeSample = ActiveSample2
                , voice1String = DrumSongs.sample2TextVoice1
                , voice2String = DrumSongs.sample2TextVoice2
                , appState = Stopped
                , clockRunning = False
                , beats = 0
              }
            , sendCommand "stop:now"
            )

        MuteVoice1 ->
            ( { model
                | appState = Stopped
                , clockRunning = False
                , beats = 0
                , voices =
                    if List.member Voice1 model.voices then
                        List.filter (\v -> v /= Voice1) model.voices

                    else
                        Voice1 :: model.voices
              }
            , if List.member Voice1 model.voices then
                sendCommand "stop:now"

              else
                sendCommand "nada"
            )

        MuteVoice2 ->
            ( { model
                | appState = Stopped
                , clockRunning = False
                , beats = 0
                , voices =
                    if List.member Voice2 model.voices then
                        List.filter (\v -> v /= Voice2) model.voices

                    else
                        Voice2 :: model.voices
              }
            , if List.member Voice2 model.voices then
                sendCommand "stop:now"

              else
                sendCommand "nada"
            )

        ReadVoice1 str ->
            ( { model | voice1String = str }, Cmd.none )

        ReadVoice2 str ->
            ( { model | voice2String = str }, Cmd.none )

        InputBPM str ->
            ( { model
                | appState = Stopped
                , clockRunning = False
                , bpmString = str
                , bpm = String.toFloat str |> Maybe.withDefault 72.0
              }
            , sendCommand "stop:now"
            )

        SetTempo ->
            ( { model | bpm = String.toFloat model.bpmString |> Maybe.withDefault 72.0 }
            , sendCommand <| "tempo:" ++ adjustBPMString model.bpmString
            )

        Play ->
            if model.appState == Playing then
                ( model, Cmd.none )

            else
                let
                    noteList1 : List String
                    noteList1 =
                        Phoneme.toPitchNameListTransposeOctave -1 model.voice1String

                    noteList2 =
                        Phoneme.toPitchNameList model.voice2String

                    part1 : Player.Part
                    part1 =
                        if List.member Voice1 model.voices then
                            Player.partFromMelody "4n" "0.8" noteList1

                        else
                            Player.partFromMelody "4n" "0.8" []

                    part2 : Player.Part
                    part2 =
                        if List.member Voice2 model.voices then
                            Player.partFromMelody "4n" "0.4" noteList2

                        else
                            Player.partFromMelody "4n" "0.4" []

                    parts =
                        [ part1, part2 ]

                    voiceToPart : Voice -> Player.Part
                    voiceToPart voice =
                        case voice of
                            Voice1 ->
                                part1

                            Voice2 ->
                                part2

                    piece =
                        { bpm = String.toInt model.bpmString |> Maybe.withDefault 72
                        , parts = parts
                        }
                in
                ( { model
                    | appState = Playing
                    , clockRunning = True
                    , beats = 0
                    , notesForVoice1 = noteList1 |> List.take 30 |> String.join " "
                    , notesForVoice2 = noteList2 |> List.take 30 |> String.join " "
                  }
                , Cmd.batch
                    [ sendCommand <| "tempo:" ++ adjustBPMString model.bpmString
                    , sendPiece <| Player.encodePiece piece
                    ]
                )

        Stop ->
            ( { model | appState = Stopped, clockRunning = False }
            , sendCommand "stop:now"
            )

        DoReset ->
            ( { model | appState = Reset, clockRunning = False }, sendCommand "reset:now" )


adjustBPMString : String -> String
adjustBPMString str =
    str
        |> String.toInt
        |> Maybe.withDefault 72
        |> toFloat
        |> (\x -> x / 2)
        |> String.fromFloat



-- VIEW


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ Element.focusStyle noFocus ] }
        [ Background.color Color.windowBGColor
        , Font.color (Element.rgb 1 1 1)
        ]
        (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "Techno Drum Language App"
            , readVoice1 model
            , readVoice2 model
            , row [ Element.spacing 24, centerX ] [ displayPeriod model, displayClock model ]
            , appButtons model
            , row [ Element.paddingXY 0 8, Element.spacing 24, Font.size 14 ]
                [ newTabLink [ alignLeft, Font.size 12, paddingXY 18 0 ]
                    { url = "https://jxxcarlson.io/posts/2019-06-29-drum-language/"
                    , label = el [ Font.underline, Font.color Color.text ] (text "Article")
                    }
                , Element.el [ Font.size 14, Font.color (Color.gray 180) ] (Element.text "Try with good earphones or speaker.  You'll be surprised!")
                , Element.el [ Font.size 14, Font.color (Color.gray 180) ] (Element.text "Period and beat count in quarter notes.")
                ]
            ]
        ]


displayClock : Model -> Element Msg
displayClock model =
    Element.el [ Font.size 12, Font.color Color.text ] (text <| "Page, Measure, Beat: " ++ String.fromInt (model.beats // (4 * 16)) ++ ", " ++ String.fromInt (model.beats // 4) ++ ", " ++ String.fromInt model.beats)


displayPeriod : Model -> Element Msg
displayPeriod model =
    let
        n1 =
            List.length <| Phoneme.toPitchNameList model.voice1String

        n2 =
            List.length <| Phoneme.toPitchNameList model.voice2String
    in
    case Rational.lcm (2 * n1) n2 of
        Nothing ->
            el [ Font.size 12, centerX ] (text <| "Period: --")

        Just k ->
            row [ centerX ]
                [ el [ Font.size 12, centerX ] (text <| "Period : " ++ String.fromInt (k // 2))
                ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.size 24 ] [ text str ]


displayVoice : String -> List String -> Element msg
displayVoice label noteList =
    let
        noteLisAsString =
            String.join " " (List.take 20 noteList)

        tag =
            if List.length noteList > 20 then
                " ..."

            else
                ""
    in
    row [ centerX, Font.size 11 ]
        [ text <| label ++ " " ++ String.fromInt (List.length noteList) ++ ", notes: " ++ noteLisAsString ++ tag ]


readVoice1 : Model -> Element Msg
readVoice1 model =
    column [ spacing 8 ]
        [ row [ Element.spacing 12 ] [ el [ Font.bold, Font.size 14 ] (text <| "Voice 1"), muteVoice1 model ]
        , Input.multiline multilineInputStyle
            { onChange = ReadVoice1
            , text = model.voice1String
            , placeholder = Nothing
            , label = Input.labelLeft [] <| el [] (text "")
            , spellcheck = False
            }
        , displayVoice "beats (quarters):" (Phoneme.toPitchNameList model.voice1String)
        ]


multilineInputStyle =
    [ width (px 700)
    , height (px 180)
    , Font.size 14
    , Font.color (rgb255 0 0 0)
    , Background.color (rgb 1 1 1)
    , Border.width 1
    , Border.color (rgb255 0 0 0)
    ]


readVoice2 : Model -> Element Msg
readVoice2 model =
    column [ spacing 8 ]
        [ row [ Element.spacing 12 ] [ el [ Font.bold, Font.size 14 ] (text <| "Voice 2"), muteVoice2 model ]
        , Input.multiline multilineInputStyle
            { onChange = ReadVoice2
            , text = model.voice2String
            , placeholder = Nothing
            , label = Input.labelLeft [] <| el [] (text "")
            , spellcheck = False
            }
        , displayVoice "beats (eighths):" (Phoneme.toPitchNameList model.voice2String)
        ]


inputBPM : Model -> Element Msg
inputBPM model =
    Input.text
        [ width (px 60)
        , height (px 35)
        , Font.size 16
        , Font.color Color.inputText
        , Element.htmlAttribute <| Html.Events.on "keydown" (enterKeyDecoder SetTempo)
        ]
        { onChange = InputBPM
        , text = model.bpmString
        , placeholder = Nothing

        --, label = Input.labelLeft [ Font.color Color.text ] <| el [ moveDown 10, paddingXY 4 0 ] (text "BPM")
        , label = Input.labelHidden "BPM input"
        }


enterKeyDecoder : Msg -> Decode.Decoder Msg
enterKeyDecoder msg =
    Html.Events.keyCode
        |> Decode.andThen
            (\code ->
                if code == 13 then
                    Decode.succeed msg

                else
                    Decode.fail "Not the Enter key"
            )


appButtons : Model -> Element Msg
appButtons model =
    row [ centerX, spacing 20 ]
        [ instructionsButton model
        , sampleButton1 model
        , sampleButton2 model
        , Input.button (activeButtonStyle (model.appState == Playing))
            { onPress = Just Play
            , label = el [ centerX, centerY ] (text "Play")
            }
        , Input.button (activeButtonStyle (model.appState == Stopped))
            { onPress = Just Stop
            , label = el [ centerX, centerY ] (text "Stop")
            }
        , Input.button (activeButtonStyle (model.appState == Reset))
            { onPress = Just DoReset
            , label = el [ centerX, centerY ] (text "Reset")
            }
        , inputBPM model
        ]


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


instructionsButton model =
    Input.button (activeButtonStyle (model.activeSample == ActiveSampleNone))
        { onPress = Just Instructions
        , label = el [ centerX, centerY ] (text "Instructions")
        }


activeButtonStyle predicate =
    if predicate then
        buttonStyleSelected

    else
        buttonStyle


activeButtonStyle2 predicate =
    if predicate then
        buttonStyleSelectedSmall

    else
        buttonStyleSmall


sampleButton1 model =
    Input.button (activeButtonStyle (ActiveSample1 == model.activeSample))
        { onPress = Just Sample1
        , label = el [ centerX, centerY ] (text "Sample 1")
        }


sampleButton2 model =
    Input.button (activeButtonStyle (ActiveSample2 == model.activeSample))
        { onPress = Just Sample2
        , label = el [ centerX, centerY ] (text "Sample 2")
        }


muteVoice1 model =
    let
        labelTitle =
            if List.member Voice1 model.voices then
                "Active"

            else
                "Muted"
    in
    Input.button (activeButtonStyle2 (List.member Voice1 model.voices))
        { onPress = Just MuteVoice1
        , label = el [ centerX, centerY ] (text labelTitle)
        }


muteVoice2 model =
    let
        labelTitle =
            if List.member Voice2 model.voices then
                "Active"

            else
                "Muted"
    in
    Input.button (activeButtonStyle2 (List.member Voice2 model.voices))
        { onPress = Just MuteVoice2
        , label = el [ centerX, centerY ] (text labelTitle)
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
    , Background.color Color.appBGColor
    , Font.color Color.buttonText
    , appBorder
    , Border.width 1
    , width (px 800)
    , paddingXY 40 40
    , Font.color (rgb255 255 255 255)
    ]


buttonStyle =
    [ Background.color Color.buttonBGColor
    , Font.color Color.text
    , Font.color Color.buttonText
    , largeButtonBorder
    , Font.size 16
    , paddingXY 15 8
    ]


buttonStyleSelected =
    [ Background.color Color.red
    , Font.color Color.buttonText
    , largeButtonBorder
    , Font.size 16
    , paddingXY 15 8
    ]


buttonStyleSmall =
    [ Background.color Color.buttonBGColor
    , Font.color Color.buttonText
    , smallButtonBorder
    , Font.size 12
    , paddingXY 4 2
    ]


buttonStyleSelectedSmall =
    [ Background.color (rgb255 140 30 30)
    , Font.color Color.buttonText
    , smallButtonBorder
    , Font.size 12
    , paddingXY 4 2
    ]


smallButtonBorder =
    Border.rounded 4


largeButtonBorder =
    Border.rounded 12


appBorder =
    Border.rounded 24
