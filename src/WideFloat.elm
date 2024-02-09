module WideFloat exposing
    ( WideFloat, Content
    , create, fromFloat
    , isZero, isLargerThan, isSmallerThan, isEqualTo
    , add, differenceFrom, multiply, multiplyFloat, dividedBy
    , proportionOf, getInternal, toString
    )

{-| This module provides functionalities of expressing and calcurating floating point numbers which has 32 exponent bits (21 bits more than ordinary `Float`).

The number of significand bit is 52 (the same as ordinary `Float`).

This module is not intended to cover all the specs included in IEEE754, especially about dealing with non normalized numbers like `NaN` and `Infinity`.
The behavior at overflowing, underflowing, division by 0, and creation from `NaN` is undefined, and may differ depending on the published version.

Please notify the author if you have some usecase that needs additional functionalities than implemented here.


# Types

@docs WideFloat, Content


# Creation

@docs create, fromFloat


# Checks

@docs isZero, isLargerThan, isSmallerThan, isEqualTo


# Calcuration

@docs add, differenceFrom, multiply, multiplyFloat, dividedBy


# Output

@docs proportionOf, getInternal, toString

-}

import Bitwise
import Round


{-| Record type that represents inner content of `WideFloat`.
-}
type alias Content =
    { base2exponent : Int
    , significand : Float
    }


{-| Opaque type that represents floating point number with 32 exponent bits and 52 significand bits.
-}
type WideFloat
    = WideFloat Content


zero =
    WideFloat
        { base2exponent = -1073741824
        , significand = 0
        }


p0 : Float
p0 =
    2


p1 : Float
p1 =
    p0 * p0


p2 : Float
p2 =
    p1 * p1


p3 : Float
p3 =
    p2 * p2


p4 : Float
p4 =
    p3 * p3


p5 : Float
p5 =
    p4 * p4


p6 : Float
p6 =
    p5 * p5


p7 : Float
p7 =
    p6 * p6


p8 : Float
p8 =
    p7 * p7


p9 : Float
p9 =
    p8 * p8


q0 : Float
q0 =
    0.5


q1 : Float
q1 =
    q0 * q0


q2 : Float
q2 =
    q1 * q1


q3 : Float
q3 =
    q2 * q2


q4 : Float
q4 =
    q3 * q3


q5 : Float
q5 =
    q4 * q4


q6 : Float
q6 =
    q5 * q5


q7 : Float
q7 =
    q6 * q6


q8 : Float
q8 =
    q7 * q7


q9 : Float
q9 =
    q8 * q8


q10 : Float
q10 =
    q9 * q9


power30 : Float
power30 =
    p5 * 0.25


fix : Content -> WideFloat
fix c =
    let
        sgn =
            if c.significand > 0 then
                1

            else
                -1

        abs =
            c.significand * sgn
    in
    if abs >= 2 then
        let
            a9 =
                if abs - p9 >= 0 then
                    abs * q9

                else
                    abs

            a8 =
                if a9 - p8 >= 0 then
                    a9 * q8

                else
                    a9

            a7 =
                if a8 - p7 >= 0 then
                    a8 * q7

                else
                    a8

            a6 =
                if a7 - p6 >= 0 then
                    a7 * q6

                else
                    a7

            a5 =
                if a6 - p5 >= 0 then
                    a6 * q5

                else
                    a6

            a4 =
                if a5 - p4 >= 0 then
                    a5 * q4

                else
                    a5

            a3 =
                if a4 - p3 >= 0 then
                    a4 * q3

                else
                    a4

            a2 =
                if a3 - p2 >= 0 then
                    a3 * q2

                else
                    a3

            a1 =
                if a2 - p1 >= 0 then
                    a2 * q1

                else
                    a2

            a0 =
                if a1 - p0 >= 0 then
                    a1 * q0

                else
                    a1

            e =
                (if abs - p9 >= 0 then
                    512

                 else
                    0
                )
                    + (if a9 - p8 >= 0 then
                        256

                       else
                        0
                      )
                    + (if a8 - p7 >= 0 then
                        128

                       else
                        0
                      )
                    + (if a7 - p6 >= 0 then
                        64

                       else
                        0
                      )
                    + (if a6 - p5 >= 0 then
                        32

                       else
                        0
                      )
                    + (if a5 - p4 >= 0 then
                        16

                       else
                        0
                      )
                    + (if a4 - p3 >= 0 then
                        8

                       else
                        0
                      )
                    + (if a3 - p2 >= 0 then
                        4

                       else
                        0
                      )
                    + (if a2 - p1 >= 0 then
                        2

                       else
                        0
                      )
                    + (if a1 - p0 >= 0 then
                        1

                       else
                        0
                      )
                    + c.base2exponent
        in
        WideFloat
            { base2exponent = e
            , significand = a0 * sgn
            }

    else if abs <= 0 then
        zero

    else if abs < 1 then
        let
            a10 =
                if abs - q10 < 0 then
                    p9 * abs * p9

                else
                    abs

            a9 =
                if a10 - q9 < 0 then
                    a10 * p9

                else
                    a10

            a8 =
                if a9 - q8 < 0 then
                    a9 * p8

                else
                    a9

            a7 =
                if a8 - q7 < 0 then
                    a8 * p7

                else
                    a8

            a6 =
                if a7 - q6 < 0 then
                    a7 * p6

                else
                    a7

            a5 =
                if a6 - q5 < 0 then
                    a6 * p5

                else
                    a6

            a4 =
                if a5 - q4 < 0 then
                    a5 * p4

                else
                    a5

            a3 =
                if a4 - q3 < 0 then
                    a4 * p3

                else
                    a4

            a2 =
                if a3 - q2 < 0 then
                    a3 * p2

                else
                    a3

            a1 =
                if a2 - q1 < 0 then
                    a2 * p1

                else
                    a2

            a0 =
                if a1 - q0 < 0 then
                    a1 * p0

                else
                    a1

            a =
                if a0 < 1 then
                    a0 * 2

                else
                    a0

            e =
                (if abs - q10 < 0 then
                    -1024

                 else
                    0
                )
                    + (if a10 - q9 < 0 then
                        -512

                       else
                        0
                      )
                    + (if a9 - q8 < 0 then
                        -256

                       else
                        0
                      )
                    + (if a8 - q7 < 0 then
                        -128

                       else
                        0
                      )
                    + (if a7 - q6 < 0 then
                        -64

                       else
                        0
                      )
                    + (if a6 - q5 < 0 then
                        -32

                       else
                        0
                      )
                    + (if a5 - q4 < 0 then
                        -16

                       else
                        0
                      )
                    + (if a4 - q3 < 0 then
                        -8

                       else
                        0
                      )
                    + (if a3 - q2 < 0 then
                        -4

                       else
                        0
                      )
                    + (if a2 - q1 < 0 then
                        -2

                       else
                        0
                      )
                    + (if a1 - q0 < 0 then
                        -1

                       else
                        0
                      )
                    + (if a0 - 1 < 0 then
                        -1

                       else
                        0
                      )
                    + c.base2exponent
        in
        WideFloat
            { base2exponent = e
            , significand = a * sgn
            }

    else
        WideFloat c


{-| Creates a `WideFloat` value from specified exponent and significand. If the absolute value of given significand is greater 2 or less than 1, _n_ is added to the exponent and _2^(-n)_ is multiplied to the significand where _n_ denote `floor (logBase 2 (abs significand))`

    create { base2exponent = 0, significand = 10 }
        == create { base2exponent = 3, significand = 1.25 }
        --> True

-}
create : Content -> WideFloat
create a =
    fix a


{-| Creates a `WideFloat` value from specified float

    fromFloat (1024.0 * 1024.0 * 1024.0 * 1024.0)
        == create { base2exponent = 40, significand = 1 }
        --> True

-}
fromFloat : Float -> WideFloat
fromFloat f =
    fix
        { base2exponent = 0
        , significand = f
        }


{-| adds two `WideFloat`s.

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 99
            , significand = 1
            }

    add w1 w2 --> create { base2exponent = 100, significand = 1.5 }

-}
add : WideFloat -> WideFloat -> WideFloat
add (WideFloat w1) (WideFloat w2) =
    let
        small =
            if w1.base2exponent - w2.base2exponent < 0 then
                w1

            else
                w2

        large =
            if w1.base2exponent - w2.base2exponent < 0 then
                w2

            else
                w1

        de =
            large.base2exponent - small.base2exponent
    in
    if de == 0 && small.base2exponent * large.base2exponent > 0 then
        WideFloat
            { base2exponent = small.base2exponent + 1
            , significand = (small.significand + large.significand) / 2
            }

    else if de >= 64 then
        WideFloat large

    else
        let
            s =
                (if Bitwise.and 32 de > 0 then
                    q5

                 else
                    1
                )
                    * (if Bitwise.and 16 de > 0 then
                        q4

                       else
                        1
                      )
                    * (if Bitwise.and 8 de > 0 then
                        q3

                       else
                        1
                      )
                    * (if Bitwise.and 4 de > 0 then
                        q2

                       else
                        1
                      )
                    * (if Bitwise.and 2 de > 0 then
                        q1

                       else
                        1
                      )
                    * (if Bitwise.and 1 de > 0 then
                        q0

                       else
                        1
                      )
                    * small.significand
                    + large.significand
        in
        fix
            { base2exponent = large.base2exponent
            , significand = s
            }


{-| Subtracts `WideFloat` value given as the first parameter from the second. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 99
            , significand = 1.75
            }

    w1 |> differenceFrom w2 --> create { base2exponent = 97, significand = 1.0 }

-}
differenceFrom : WideFloat -> WideFloat -> WideFloat
differenceFrom (WideFloat subtrahend) (WideFloat minuend) =
    add (WideFloat { base2exponent = subtrahend.base2exponent, significand = -subtrahend.significand }) (WideFloat minuend)


{-| Multiplies two `WideFloat`s.

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 100
            , significand = 1.5
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 200
            , significand = 1.75
            }

    multiply w1 w2 --> create { base2exponent = 301, significand = 1.3125 }

-}
multiply : WideFloat -> WideFloat -> WideFloat
multiply (WideFloat w1) (WideFloat w2) =
    let
        s =
            w1.significand * w2.significand

        a =
            if s > 0 then
                s

            else
                -s
    in
    if a < 2 then
        WideFloat
            { base2exponent = w1.base2exponent + w2.base2exponent
            , significand = s
            }

    else
        WideFloat
            { base2exponent = w1.base2exponent + w2.base2exponent + 1
            , significand = s * 0.5
            }


{-| multiplies a `Float` value on a `WideFloat` value.

    w : WideFloat
    w =
        create
            { base2exponent = 100
            , significand = 1
            }

    multiplyFloat 3 w --> create { base2exponent = 101, significand = 1.5 }

-}
multiplyFloat : Float -> WideFloat -> WideFloat
multiplyFloat f (WideFloat w) =
    if f > 1 || f < -1 then
        -- must avoid overflow
        fix
            { base2exponent = w.base2exponent + 1
            , significand = f * 0.5 * w.significand
            }

    else
        fix
            { base2exponent = w.base2exponent
            , significand = f * w.significand
            }


{-| Returns ratio between to `WideFloat`s. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 99
            , significand = 1.6
            }

    w1 |> dividedBy w2
        |> differenceFrom ( create { base2exponent = 0, significand = 1.25})
        |> isSmallerThan ( create { base2exponent = -30, significand = 1})--> True

-}
dividedBy : WideFloat -> WideFloat -> WideFloat
dividedBy (WideFloat divisor) (WideFloat dividend) =
    fix
        { base2exponent = dividend.base2exponent - divisor.base2exponent
        , significand = dividend.significand / divisor.significand
        }


{-| Returns `True` if the parameter is recognized as zero in floating point precision.

    fromFloat 1
        |> add
            ( create
                { base2exponent = 100
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { base2exponent = 100
                , significand = 1
                }
            )
        |> isZero --> True


    fromFloat 1
        |> add
            ( create
                { base2exponent = 50
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { base2exponent = 50
                , significand = 1
                }
            )
        |> isZero --> False

-}
isZero : WideFloat -> Bool
isZero (WideFloat w) =
    w.significand == 0


{-| Returns `True` if the second parameter is larger than first one. Reads naturally when used with pipe operator(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 999
            , significand = 1.999
            }

    w1 |> isLargerThan w2 --> True

    w2 |> isLargerThan w1 --> False

-}
isLargerThan : WideFloat -> WideFloat -> Bool
isLargerThan (WideFloat smaller) (WideFloat larger) =
    let
        de =
            larger.base2exponent - smaller.base2exponent
    in
    if de > 0 then
        larger.significand > 0

    else if de < 0 then
        smaller.significand < 0

    else
        larger.significand - smaller.significand > 0


{-| Returns `True` if the second parameter is smaller than first one. Reads naturally when used with pipe operator(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 999
            , significand = 1.999
            }

    w1 |> isSmallerThan w2 --> False

    w2 |> isSmallerThan w1 --> True

-}
isSmallerThan : WideFloat -> WideFloat -> Bool
isSmallerThan l s =
    isLargerThan s l


{-| Returns `True` if the value of first parameter is the same as the second parameter.

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 50
            , significand = 1
            }
            |> add
                (create
                    { base2exponent = 0
                    , significand = 1
                    }
                )

    w1 |> isEqualTo w1 --> True

    w1 |> isEqualTo w2 --> False

-}
isEqualTo : WideFloat -> WideFloat -> Bool
isEqualTo w1 w2 =
    not (isLargerThan w1 w2 || isLargerThan w2 w1)


{-| Returns proportion of the first parameter in the sum of first and second parameters.

    w1 : WideFloat
    w1 =
        create
            { base2exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2exponent = 99
            , significand = 1
            }

    proportionOf w1 w2 == 2.0 / 3.0 --> True

-}
proportionOf : WideFloat -> WideFloat -> Float
proportionOf (WideFloat w1) (WideFloat w2) =
    let
        de =
            w1.base2exponent - w2.base2exponent
    in
    if de >= 60 then
        let
            s2 =
                toFloat (2 ^ -de) * w2.significand
        in
        w1.significand / (w1.significand + s2)

    else if de >= 0 then
        let
            s1 =
                if de >= 30 then
                    toFloat (Bitwise.shiftLeftBy (de - 30) 1)
                        * power30
                        * w1.significand

                else
                    toFloat (Bitwise.shiftLeftBy de 1)
                        * w1.significand
        in
        s1 / (s1 + w2.significand)

    else if de <= -60 then
        let
            s1 =
                toFloat (2 ^ de) * w1.significand
        in
        s1 / (s1 + w2.significand)

    else
        let
            s2 =
                if de <= -30 then
                    toFloat (Bitwise.shiftLeftBy (-de - 30) 1)
                        * power30
                        * w2.significand

                else
                    toFloat (Bitwise.shiftLeftBy -de 1)
                        * w2.significand
        in
        w1.significand / (w1.significand + s2)


{-| Returns a record representing the inner content of `WideFloat`.
Inverse function of `create` (when significand is specified properly).

    content : { base2exponent : Int, significand : Float}
    content =
        { base2exponent = 1000, significand = 1.234 }


    getInternal (create content)
       == content --> True

-}
getInternal : WideFloat -> Content
getInternal (WideFloat c) =
    c


{-| Returns a `String` value representing the base 10 expression of a `WideFloat`. You can specify how many digits is shown after decimal point as the first parameter. The result may be a little less precise than other calcurations implemented in this module.

    create
        { base2exponent = 2
        , significand = 1
        }
        |> toString 3 --> "4.000e0"

If you want some other formats, you can make your original `toString` function using `getInternal`.

-}
toString : Int -> WideFloat -> String
toString digits (WideFloat w) =
    if isZero (WideFloat w) then
        "0"

    else
        let
            sgn =
                if w.significand >= 0 then
                    1

                else
                    -1

            log10 =
                logBase 10 (sgn * w.significand)
                    + toFloat w.base2exponent
                    * 0.3010299956639812

            base10exponent =
                floor log10

            base10significand =
                (10.0 ^ (log10 - toFloat base10exponent)) * sgn
        in
        Round.round digits base10significand
            ++ "e"
            ++ String.fromInt base10exponent
