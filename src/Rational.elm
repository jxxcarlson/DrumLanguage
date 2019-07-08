module Rational exposing (Rational(..), add, div, gcd, inject, max, mul, project, reduce, sub, sum)


type Rational
    = R Int Int


reduce : Rational -> Rational
reduce (R a b) =
    let
        g =
            gcd (abs a) (abs b)
    in
    if b > 0 then
        R (a // g) (b // g)

    else
        R (-a // g) (-b // g)


mul : Rational -> Rational -> Rational
mul (R a b) (R c d) =
    reduce <| R (a * c) (b * d)


div : Rational -> Rational -> Maybe Rational
div (R a b) (R c d) =
    if c == 0 then
        Nothing

    else
        Just <| reduce <| R (a * d) (b * c)


add : Rational -> Rational -> Rational
add (R a b) (R c d) =
    let
        num =
            a * d + b * c

        denom =
            b * d
    in
    reduce <| R num denom


max : Rational -> Rational -> Rational
max p q =
    let
        (R a b) =
            p

        (R c d) =
            q
    in
    if a * d > b * c then
        p

    else
        q


{-|

> sum [(R 1 2), (R 1 3), (R 1 5)]
> R 31 30 : Rational

-}
sum : List Rational -> Rational
sum list =
    List.foldl (\x acc -> add x acc) (R 0 1) list


sub : Rational -> Rational -> Rational
sub (R a b) (R c d) =
    let
        num =
            a * d - b * c

        denom =
            b * d
    in
    reduce <| R num denom


inject : Int -> Rational
inject n =
    R n 1


project : Rational -> Maybe Int
project (R a_ b_) =
    let
        (R a b) =
            reduce (R a_ b_)
    in
    if b == 1 then
        Just a

    else
        Nothing


gcd : Int -> Int -> Int
gcd a b =
    let
        r =
            modBy a b
    in
    if r == 0 then
        a

    else
        gcd r a
