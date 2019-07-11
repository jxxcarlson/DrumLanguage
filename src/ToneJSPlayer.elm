module ToneJSPlayer exposing (encodeEventList, encodeParts, eventListOfMusic)

import Duration exposing (Duration)
import Json.Encode as Encode
import Music exposing (Music(..))
import Pitch exposing (Pitch)
import Primitive exposing (Primitive(..))
import Rational exposing (Rational)


type alias TimeCode =
    String


type alias TJSPitch =
    String


type alias TJSDuration =
    String


type alias BPM =
    Int


type alias Rate =
    Float


type alias TransportTime =
    Rational


type alias Event =
    { time : TimeCode
    , note : TJSPitch
    , duration : TJSDuration
    }


encodeEvent : Event -> Encode.Value
encodeEvent event =
    Encode.object
        [ ( "time", Encode.string event.time )
        , ( "note", Encode.string event.note )
        , ( "dur", Encode.string event.duration )
        ]


{-|

> eventListOfMusic 60 m |> encodeEventList
> <internals> : Json.Encode.Value

-}
encodeEventList : List Event -> Encode.Value
encodeEventList eventList =
    Encode.list encodeEvent eventList


encodeParts : List (List Event) -> Encode.Value
encodeParts parts =
    Encode.object
        [ ( "numberOfParts", Encode.int (List.length parts) )
        , ( "parts", Encode.list encodeEventList parts )
        ]


realTime : Rate -> Duration -> Float
realTime rate dur =
    rate * Rational.realValue dur


realTimeAsString : Rate -> Duration -> String
realTimeAsString rate dur =
    String.fromFloat (realTime rate dur)


{-|

> m = sequence [c 3 hn, d 3 qn]
> Sequence [Prim (Note (R 1 2) (C,3)),Prim (Note (R 1 4) (D,3))]

> eventListOfMusic 60 m
> [{ duration = "0.5", note = "C3", time = "0" },{ duration = "0.25", note = "D3", time = "0.5" }]

    : List Event

-}
eventArrayOfMusicAtTime1 : TransportTime -> BPM -> Music Pitch -> List Event
eventArrayOfMusicAtTime1 tt bpm music =
    let
        rate =
            60.0 / toFloat bpm

        list =
            Music.flatten music

        folder : Primitive Pitch -> ( Rational, List Event ) -> ( Rational, List Event )
        folder =
            \prim ( tt_, acc ) ->
                ( Rational.add tt_ (Primitive.duration prim)
                , eventOfPrimitive tt_ rate prim :: acc
                )
    in
    List.foldl folder ( Rational.zero, [] ) list |> Tuple.second |> List.reverse


eventArrayOfMusic1 : BPM -> Music Pitch -> List Event
eventArrayOfMusic1 =
    eventArrayOfMusicAtTime Rational.zero


eventOfPrimitive : TransportTime -> Rate -> Primitive Pitch -> Event
eventOfPrimitive tt_ rate p =
    case p of
        Note dur pch ->
            { time = realTimeAsString rate tt_, note = Pitch.tjsStringOfPitch pch, duration = realTimeAsString rate dur }

        Rest dur ->
            { time = realTimeAsString rate tt_, note = "Silence", duration = realTimeAsString rate dur }


{-| dMinor = stack [ d 4 wn, f 4 wn, a 4 wn ]
gMajor = stack [ g 4 wn, b 4 wn, d 4 wn ]
cMajor = stack [ c 4 bn, ee 4 bn, g 4 bn ]
iiVI = sequence [ dMinor, gMajor, cMajor ]

> eventListOfMusic 60 iiVI
> [{ duration = "1", note = "A4", time = "0" },{ duration = "1", note = "F4", time = "0" },{ duration = "1", note = "D4", time = "0" },{ duration = "1", note = "D4", time = "1" },{ duration = "1", note = "B4", time = "1" },{ duration = "1", note = "G4", time = "1" },{ duration = "2", note = "G4", time = "2" },{ duration = "2", note = "E4", time = "2" },{ duration = "2", note = "C4", time = "2" }]

    : List Event

-}
eventListOfMusic : BPM -> Music Pitch -> List Event
eventListOfMusic =
    eventArrayOfMusicAtTime Rational.zero


eventArrayOfMusicAtTime : TransportTime -> BPM -> Music Pitch -> List Event
eventArrayOfMusicAtTime startTime bpm music =
    let
        rate =
            60.0 / toFloat bpm

        list =
            Music.flatten music

        folder : Music Pitch -> ( Rational, List Event ) -> ( Rational, List Event )
        folder =
            \mu ( tt_, acc ) ->
                case mu of
                    Prim p ->
                        ( Rational.add tt_ (Primitive.duration p)
                        , eventOfPrimitive tt_ rate p :: acc
                        )

                    _ ->
                        ( Rational.add tt_ (Music.duration mu), eventArrayOfMusicAtTime tt_ bpm mu ++ acc )
    in
    case music of
        Prim p ->
            [ eventOfPrimitive startTime rate p ]

        Sequence s ->
            List.foldl folder ( startTime, [] ) s |> Tuple.second |> List.reverse

        Stack s ->
            List.map (eventArrayOfMusicAtTime startTime bpm) s |> List.concat

        Modify _ _ ->
            []
