module Duration exposing (Duration, bn, dden, ddhn, ddqn, den, dhn, dqn, dsn, dtn, dwn, en, hn, qn, sfn, sn, tn, wn)

import Rational exposing (Rational(..))


type alias Duration =
    Rational


bn =
    R 2 1


wn =
    R 1 1


hn =
    R 1 2


qn =
    R 1 4


en =
    R 1 8


sn =
    R 1 16


tn =
    R 1 32


sfn =
    R 1 64


dwn =
    R 3 2


dhn =
    R 3 4


dqn =
    R 3 8


den =
    R 3 16


dsn =
    R 3 32


dtn =
    R 3 64


ddhn =
    R 7 8


ddqn =
    R 7 16


dden =
    R 7 32
