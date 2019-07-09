module ToneJSPlayer exposing (eventArrayOfMusic, eventArrayOfMusicAtTime2)

import Duration exposing (Duration)
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


realTime : Rate -> Duration -> Float
realTime rate dur =
    rate * Rational.realValue dur


realTimeAsString : Rate -> Duration -> String
realTimeAsString rate dur =
    String.fromFloat (realTime rate dur)


{-|

> m = sequence [c 3 hn, d 3 qn]
> Sequence [Prim (Note (R 1 2) (C,3)),Prim (Note (R 1 4) (D,3))]

> eventArrayOfMusic 60 m
> [{ duration = "0.5", note = "C3", time = "0" },{ duration = "0.25", note = "D3", time = "0.5" }]

    : List Event

-}
eventArrayOfMusicAtTime : TransportTime -> BPM -> Music Pitch -> List Event
eventArrayOfMusicAtTime tt bpm music =
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


eventArrayOfMusic : BPM -> Music Pitch -> List Event
eventArrayOfMusic =
    eventArrayOfMusicAtTime Rational.zero


eventOfPrimitive : TransportTime -> Rate -> Primitive Pitch -> Event
eventOfPrimitive tt_ rate p =
    case p of
        Note dur pch ->
            { time = realTimeAsString rate tt_, note = Pitch.tjsStringOfPitch pch, duration = realTimeAsString rate dur }

        Rest dur ->
            { time = realTimeAsString rate tt_, note = "Silence", duration = realTimeAsString rate dur }


eventArrayOfMusicAtTime2 : TransportTime -> BPM -> Music Pitch -> List Event
eventArrayOfMusicAtTime2 startTime bpm music =
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
                        ( Rational.add tt_ (Music.duration mu), eventArrayOfMusicAtTime2 tt_ bpm mu ++ acc )
    in
    case music of
        Prim p ->
            [ eventOfPrimitive startTime rate p ]

        Sequence s ->
            List.foldl folder ( startTime, [] ) s |> Tuple.second |> List.reverse

        Stack s ->
            List.map (eventArrayOfMusicAtTime2 startTime bpm) s |> List.concat

        Modify _ _ ->
            []
