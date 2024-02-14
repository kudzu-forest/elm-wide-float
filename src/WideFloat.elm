module WideFloat exposing
    ( WideFloat, Content
    , zero, one
    , create, fromFloat
    , isZero, isLargerThan, isSmallerThan, isEqualTo
    , add, differenceFrom, multiply, multiplyFloat, dividedBy
    , proportionOf, getInternal, toString
    )

{-| The definitions and functionalities of `WideFloat` type (which is explained in README.md).


# Types

@docs WideFloat, Content


# Constants

@docs zero, one


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
    { base2toThe1024exponent : Int
    , significand : Float
    }


{-| Opaque type that represents floating point number with 32 exponent bits and 52 significand bits.
-}
type WideFloat
    = WideFloat Content


{-| `WideFloat` representing 0.

    zero == fromFloat 0 --> True

-}
zero : WideFloat
zero =
    WideFloat
        { base2toThe1024exponent = -2147483648
        , significand = 0
        }


{-| `WideFloat` representing 1.

    one == fromFloat 1 --> True

-}
one : WideFloat
one =
    WideFloat
        { base2toThe1024exponent = 0
        , significand = 1
        }


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
    if abs - twoToThe512 >= 0 then
        WideFloat
            { base2toThe1024exponent =
                c.base2toThe1024exponent + 1
            , significand =
                (c.significand * invTwoToThe512)
                    * invTwoToThe512
            }

    else if abs - invTwoToThe512 < 0 then
        WideFloat
            { base2toThe1024exponent =
                c.base2toThe1024exponent - 1
            , significand =
                (c.significand * twoToThe512)
                    * twoToThe512
            }

    else
        WideFloat c


{-| Creates a `WideFloat` value from specified exponent and significand. If the absolute value of given significand is greater 2 or less than 1, _n_ is added to the exponent and _2^(-n)_ is multiplied to the significand where _n_ denote `floor (logBase 2 (abs significand))`

    create { base2toThe1024exponent = 0, significand = 10 }
        == create { base2toThe1024exponent = 3, significand = 1.25 }
        --> True

-}
create : Content -> WideFloat
create a =
    fix a


{-| Creates a `WideFloat` value from specified float

    fromFloat (1024.0 * 1024.0 * 1024.0 * 1024.0)
        == create { base2toThe1024exponent = 40, significand = 1 }
        --> True

-}
fromFloat : Float -> WideFloat
fromFloat f =
    fix
        { base2toThe1024exponent = 0
        , significand = f
        }


{-| adds two `WideFloat`s.

    w1 : WideFloat
    w1 =
        create
            { base2toThe1024exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 99
            , significand = 1
            }

    add w1 w2 --> create { base2toThe1024exponent = 100, significand = 1.5 }

-}
add : WideFloat -> WideFloat -> WideFloat
add (WideFloat w1) (WideFloat w2) =
    let
        de =
            w1.base2toThe1024exponent - w2.base2toThe1024exponent
    in
    if de > 0 then
        add (WideFloat w2) (WideFloat w1)

    else if de == 0 then
        fix
            { base2toThe1024exponent = w1.base2toThe1024exponent
            , significand = w1.significand + w2.significand
            }

    else if de == -1 then
        let
            s =
                ((w1.significand * invTwoToThe512)
                    + (w2.significand * twoToThe512)
                )
                    * invTwoToThe512
        in
        fix
            { base2toThe1024exponent = w2.base2toThe1024exponent
            , significand = s
            }

    else
        WideFloat w2


{-| Subtracts `WideFloat` value given as the first parameter from the second. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2toThe1024exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 99
            , significand = 1.75
            }

    w1 |> differenceFrom w2 --> create { base2toThe1024exponent = 97, significand = 1.0 }

-}
differenceFrom : WideFloat -> WideFloat -> WideFloat
differenceFrom (WideFloat subtrahend) (WideFloat minuend) =
    add (WideFloat { base2toThe1024exponent = subtrahend.base2toThe1024exponent, significand = -subtrahend.significand }) (WideFloat minuend)


{-| Multiplies two `WideFloat`s.

    w1 : WideFloat
    w1 =
        create
            { base2toThe1024exponent = 100
            , significand = 1.5
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 200
            , significand = 1.75
            }

    multiply w1 w2 --> create { base2toThe1024exponent = 301, significand = 1.3125 }

-}
multiply : WideFloat -> WideFloat -> WideFloat
multiply (WideFloat w1) (WideFloat w2) =
    fix
        { base2toThe1024exponent =
            w1.base2toThe1024exponent
                + w2.base2toThe1024exponent
        , significand =
            w1.significand
                * w2.significand
        }


{-| multiplies a `Float` value on a `WideFloat` value.

    w : WideFloat
    w =
        create
            { base2toThe1024exponent = 100
            , significand = 1
            }

    multiplyFloat 3 w --> create { base2toThe1024exponent = 101, significand = 1.5 }

-}
multiplyFloat : Float -> WideFloat -> WideFloat
multiplyFloat f (WideFloat w) =
    let
        absf =
            if f > 0 then
                f

            else
                -f

        abss =
            if w.significand > 0 then
                w.significand

            else
                -w.significand
    in
    if absf > 1 && abss > 1 then
        fix
            { base2toThe1024exponent = w.base2toThe1024exponent + 1
            , significand =
                (f * invTwoToThe512) * (w.significand * invTwoToThe512)
            }

    else if absf < 1 && abss < 1 then
        fix
            { base2toThe1024exponent = w.base2toThe1024exponent - 1
            , significand =
                (f * twoToThe512) * (w.significand * twoToThe512)
            }

    else
        fix
            { base2toThe1024exponent = w.base2toThe1024exponent
            , significand = f * w.significand
            }


{-| Returns ratio between to `WideFloat`s. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { base2toThe1024exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 99
            , significand = 1.6
            }

    w1 |> dividedBy w2
        |> differenceFrom ( create { base2toThe1024exponent = 0, significand = 1.25})
        |> isSmallerThan ( create { base2toThe1024exponent = -30, significand = 1})--> True

-}
dividedBy : WideFloat -> WideFloat -> WideFloat
dividedBy (WideFloat divisor) (WideFloat dividend) =
    fix
        { base2toThe1024exponent = dividend.base2toThe1024exponent - divisor.base2toThe1024exponent
        , significand = dividend.significand / divisor.significand
        }


{-| Returns `True` if the parameter is recognized as zero in floating point precision.

    fromFloat 1
        |> add
            ( create
                { base2toThe1024exponent = 100
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { base2toThe1024exponent = 100
                , significand = 1
                }
            )
        |> isZero --> True


    fromFloat 1
        |> add
            ( create
                { base2toThe1024exponent = 50
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { base2toThe1024exponent = 50
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
            { base2toThe1024exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 999
            , significand = 1.999
            }

    w1 |> isLargerThan w2 --> True

    w2 |> isLargerThan w1 --> False

-}
isLargerThan : WideFloat -> WideFloat -> Bool
isLargerThan (WideFloat smaller) (WideFloat larger) =
    let
        de =
            larger.base2toThe1024exponent - smaller.base2toThe1024exponent
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
            { base2toThe1024exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 999
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
            { base2toThe1024exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 50
            , significand = 1
            }
            |> add
                (create
                    { base2toThe1024exponent = 0
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
            { base2toThe1024exponent = 100
            , significand = 2 ^ (-511)
            }

    w2 : WideFloat
    w2 =
        create
            { base2toThe1024exponent = 99
            , significand = 1
            }

    proportionOf w1 w2 == 2.0 / 3.0 --> True

-}
proportionOf : WideFloat -> WideFloat -> Float
proportionOf (WideFloat w1) (WideFloat w2) =
    let
        de =
            w1.base2toThe1024exponent - w2.base2toThe1024exponent
    in
    if de > 0 then
        1 - proportionOf (WideFloat w2) (WideFloat w1)

    else if de == 0 then
        w1.significand / (w1.significand + w2.significand)

    else if de == -1 then
        let
            w1s =
                w1.significand * twoToThe512

            w2s =
                w2.significand * invTwoToThe512
        in
        w1s / (w1s + w2s)

    else
        0


{-| Returns a record representing the inner content of `WideFloat`.
Inverse function of `create` (when significand is specified properly).

    content : { base2toThe1024exponent : Int, significand : Float}
    content =
        { base2toThe1024exponent = 1000, significand = 1.234 }


    getInternal (create content)
       == content --> True

-}
getInternal : WideFloat -> Content
getInternal (WideFloat c) =
    c


{-| Returns a `String` value representing the base 10 expression of a `WideFloat`. You can specify how many digits is shown after decimal point as the first parameter. The result may be a little less precise than other calcurations implemented in this module.

    create
        { base2toThe1024exponent = 1
        , significand = 1
        }
        |> toString 3 --> "1.798e308"

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
                    + toFloat w.base2toThe1024exponent
                    -- 1024 * log_10 2
                    * 308.25471555991675

            base10exponent =
                floor log10

            base10significand =
                (10.0 ^ (log10 - toFloat base10exponent)) * sgn
        in
        Round.round digits base10significand
            ++ "e"
            ++ String.fromInt base10exponent


twoToThe32 : Float
twoToThe32 =
    4294967296


twoToThe64 : Float
twoToThe64 =
    twoToThe32 * twoToThe32


twoToThe128 : Float
twoToThe128 =
    twoToThe64 * twoToThe64


twoToThe256 : Float
twoToThe256 =
    twoToThe128 * twoToThe128


twoToThe512 : Float
twoToThe512 =
    twoToThe256 * twoToThe256


invTwoToThe512 : Float
invTwoToThe512 =
    1 / twoToThe512
