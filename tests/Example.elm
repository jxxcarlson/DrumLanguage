module Example exposing (basicTestMusic, testRational)

import Duration exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Music exposing (..)
import Pitch exposing (..)
import Primitive exposing (..)
import Rational exposing (..)
import Test exposing (..)


testRational : Test
testRational =
    describe "The Rational module"
        [ test "add" <|
            \_ ->
                Expect.equal (add (R 1 2) (R 3 4)) (R 5 4)

        -- Expect.equal is designed to be used in pipeline style, like this.
        , test "sum of list of rationals" <|
            \_ ->
                [ R 1 2, R 1 3, R 1 5 ]
                    |> sum
                    |> Expect.equal (R 31 30)
        , test "max of two rationals" <|
            \_ ->
                Expect.equal (Rational.max (R 1 3) (R 1 5)) (R 1 3)
        ]


basicTestMusic : Test
basicTestMusic =
    describe "The Music module"
        [ test "duration on Note" <|
            \_ -> Expect.equal (Primitive.duration (Note qn ( C, 3 ))) (R 1 4)
        , test "duration on Rest" <|
            \_ -> Expect.equal (Primitive.duration <| Rest qn) (R 1 4)
        , test "duration on Primitive " <|
            \_ -> Expect.equal (Music.duration <| Prim (Rest qn)) (R 1 4)
        , test "2-note phrase (L)" <|
            \_ ->
                let
                    n1 =
                        Prim <| Note qn ( C, 3 )

                    n2 =
                        Prim <| Rest qn

                    n3 =
                        Prim <| Note qn ( E, 3 )

                    music =
                        Sequence [ n1, n2, n3 ]
                in
                Expect.equal (Music.duration music) (R 3 4)
        , test "duration on 4-note phrase built with `Sequence`" <|
            \_ ->
                let
                    n1 =
                        Prim <| Note qn ( C, 3 )

                    n2 =
                        Prim <| Rest qn

                    n3 =
                        Prim <| Note qn ( E, 3 )

                    p =
                        Sequence [ n1, n2, n3 ]
                in
                Expect.equal (Music.duration (Sequence [ p, p ])) (R 3 2)
        , test "duration on 4-note phrase built with `Stack`" <|
            \_ ->
                let
                    n1 =
                        Prim <| Note qn ( C, 3 )

                    n2 =
                        Prim <| Rest qn

                    n3 =
                        Prim <| Note qn ( E, 3 )

                    p =
                        Sequence [ n1, n2, n3 ]

                    q =
                        Sequence [ n1, n2 ]
                in
                Expect.equal (Music.duration (Stack [ p, q ])) (R 3 4)
        , test "251 progression, HSM p. 35" <|
            \_ ->
                let
                    dMinor =
                        stack [ d 4 wn, f 4 wn, a 4 wn ]

                    gMajor =
                        stack [ g 4 wn, b 4 wn, d 4 wn ]

                    cMajor =
                        stack [ c 4 bn, ee 4 bn, g 4 bn ]

                    c251 =
                        sequence [ dMinor, gMajor, cMajor ]

                    c251Expected =
                        Sequence [ Stack [ Prim (Note (R 1 1) ( D, 4 )), Prim (Note (R 1 1) ( F, 4 )), Prim (Note (R 1 1) ( A, 4 )) ], Stack [ Prim (Note (R 1 1) ( G, 4 )), Prim (Note (R 1 1) ( B, 4 )), Prim (Note (R 1 1) ( D, 4 )) ], Stack [ Prim (Note (R 2 1) ( C, 4 )), Prim (Note (R 2 1) ( E, 4 )), Prim (Note (R 2 1) ( G, 4 )) ] ]
                in
                Expect.equal c251 c251Expected
        ]
