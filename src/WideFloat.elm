module WideFloat exposing
    ( WideFloat
    , create, fromFloat
    , add, differenceFrom, multiplyFloat, dividedBy
    , isZero, isLargerThan, isSmallerThan
    , proportion, getInternal, toString
    )

{-|

    Floating point number having wider range of exponent and nearly the same precision as ordinary `Float`.
    This module is for representing number like 1.0e-600 or 1.0e600, which is impossible with IEEE754 double precision floating point number.
    The minimum and maximum positive number representable with this module is about 2^(-2^30) and 2^(2^30), respectively.
    Please notify the author if you have some usecase that needs additional functionalities than implemented here.


# Types

@docs WideFloat


# Creation

@docs create, fromFloat


# Modification

@docs add, differenceFrom, multiplyFloat, dividedBy


# Query

@docs isZero, isLargerThan, isSmallerThan


# Conversion

@docs proportion, getInternal, toString

-}


type WideFloat
    = WideFloat
        { exponent : Int
        , significand : Float
        }



{- Implementation Note
   We maintain a constraint that `significand` belongs in interval [1,2).
   `exponent` is true exponent value plus 2^30 - 1.
-}


{-| Creates a `WideFloat` value from specified exponent and significand. If the given significand is greater 2 or less than 1, _n_ is added to the exponent and _2^(-n)_ is multiplied to the significand where _n_ denote _floor (logBase 2 (abs significand))_

    create { exponent = 0, significand = 10 }
        == create { exponent = 3, significand = 1.25 }
        --> True

-}
create : { exponent : Int, significand : Float } -> WideFloat
create f =
    if f.significand == 0 then
        zero

    else
        let
            significandExponent =
                floor (logBase 4 (f.significand * f.significand))

            finalSignificand =
                f.significand * (0.5 ^ toFloat significandExponent)

            finalExponent =
                significandExponent + f.exponent
        in
        WideFloat
            { exponent = finalExponent
            , significand = finalSignificand
            }


{-| Creates a `WideFloat` value from specified float

    fromFloat (1024.0 * 1024.0 * 1024.0 * 1024.0)
        == create { exponent = 40, significand = 1 }
        --> True

-}
fromFloat : Float -> WideFloat
fromFloat f =
    let
        significandExponent =
            floor (logBase 4 (f * f))

        finalSignificand =
            f * (0.5 ^ toFloat significandExponent)

        finalExponent =
            significandExponent
    in
    WideFloat
        { exponent = finalExponent
        , significand = finalSignificand
        }


zero : WideFloat
zero =
    WideFloat
        { exponent = -2147483648
        , significand = 0
        }


{-| adds tow `WideFloat`s.

    w1 : WideFloat
    w1 =
        create
            { exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 99
            , significand = 1
            }

    add w1 w2 --> create { exponent = 100, significand = 1.5 }

-}
add : WideFloat -> WideFloat -> WideFloat
add (WideFloat w1) (WideFloat w2) =
    if w1.exponent < w2.exponent then
        add (WideFloat w2) (WideFloat w1)

    else if
        (w1.exponent - w2.exponent == 0)
            && (w1.significand + w2.significand == 0)
    then
        zero

    else
        let
            exponentDiff =
                w1.exponent - w2.exponent

            significand2 =
                (0.5 ^ toFloat exponentDiff) * w2.significand

            newSignificand =
                w1.significand + significand2

            significandExponent =
                floor (logBase 4 (newSignificand * newSignificand))

            finalSignificand =
                (0.5 ^ toFloat significandExponent) * newSignificand

            finalExponent =
                significandExponent + w1.exponent
        in
        WideFloat
            { exponent = finalExponent
            , significand = finalSignificand
            }


{-| multiplies a `Float` value on a `WideFloat` value.

    w : WideFloat
    w =
        create
            { exponent = 100
            , significand = 1
            }

    multiplyFloat 3 w --> create { exponent = 101, significand = 1.5 }

-}
multiplyFloat : Float -> WideFloat -> WideFloat
multiplyFloat f (WideFloat w) =
    if f == 0 then
        zero

    else
        let
            newSignificand =
                w.significand * f

            significandExponent =
                floor (logBase 4 (newSignificand * newSignificand))

            finalSignificand =
                (0.5 ^ toFloat significandExponent) * newSignificand

            finalExponent =
                significandExponent + w.exponent
        in
        WideFloat
            { exponent = finalExponent
            , significand = finalSignificand
            }


{-| Subtracts `WideFloat` value given as the first parameter from the second. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 99
            , significand = 1.75
            }

    w1 |> differenceFrom w2 --> create { exponent = 97, significand = 1.0 }

-}
differenceFrom : WideFloat -> WideFloat -> WideFloat
differenceFrom w1 w2 =
    add (multiplyFloat -1 w1) w2


{-| Returns proportion of the first parameter in the sum of first and second parameters.

    w1 : WideFloat
    w1 =
        create
            { exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 99
            , significand = 1
            }

    proportion w1 w2 == 2.0 / 3.0 --> True

-}
proportion : WideFloat -> WideFloat -> Float
proportion (WideFloat w1) (WideFloat w2) =
    let
        b =
            w2.significand

        a =
            2.0 ^ toFloat (w1.exponent - w2.exponent) * w1.significand
    in
    a / (a + b)


{-| Returns ratio between to `WideFloat`s. Reads naturally when used with pipe(`|>`).

    w1 : WideFloat
    w1 =
        create
            { exponent = 100
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 99
            , significand = 1.6
            }

    w1 |> dividedBy w2
        |> differenceFrom ( create { exponent = 0, significand = 1.25})
        |> isSmallerThan ( create { exponent = -30, significand = 1})--> True

-}
dividedBy : WideFloat -> WideFloat -> WideFloat
dividedBy (WideFloat divisor) (WideFloat dividant) =
    let
        newSignificand =
            dividant.significand / divisor.significand

        significandExponent =
            floor (logBase 4 (newSignificand * newSignificand))

        finalSignificand =
            (0.5 ^ toFloat significandExponent) * newSignificand

        finalExponent =
            significandExponent + dividant.exponent - divisor.exponent
    in
    WideFloat
        { exponent = finalExponent
        , significand = finalSignificand
        }


{-| Returns `True` if the parameter is recognized as zero in floating point precision.

    fromFloat 1
        |> add
            ( create
                { exponent = 100
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { exponent = 100
                , significand = 1
                }
            )
        |> isZero --> True


    fromFloat 1
        |> add
            ( create
                { exponent = 50
                , significand = 1
                }
            )
        |> differenceFrom
            ( create
                { exponent = 50
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
            { exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 999
            , significand = 1.999
            }

    w1 |> isLargerThan w2 --> True

    w2 |> isLargerThan w1 --> False

-}
isLargerThan : WideFloat -> WideFloat -> Bool
isLargerThan (WideFloat smaller) (WideFloat larger) =
    if larger.significand > 0 then
        if smaller.significand > 0 then
            if larger.exponent == smaller.exponent then
                larger.significand > smaller.significand

            else
                larger.exponent > smaller.exponent

        else
            True

    else if smaller.significand > 0 then
        False

    else if larger.exponent == smaller.exponent then
        larger.significand < smaller.significand

    else
        larger.exponent < smaller.exponent


{-| Returns `True` if the second parameter is smaller than first one. Reads naturally when used with pipe operator(`|>`).

    w1 : WideFloat
    w1 =
        create
            { exponent = 1000
            , significand = 1
            }

    w2 : WideFloat
    w2 =
        create
            { exponent = 999
            , significand = 1.999
            }

    w1 |> isSmallerThan w2 --> False

    w2 |> isSmallerThan w1 --> True

-}
isSmallerThan : WideFloat -> WideFloat -> Bool
isSmallerThan w1 w2 =
    isLargerThan w2 w1


{-| Returns a record representing the inner content of `WideFloat`.
Inverse function of `create` (when significand is specified properly).

    content : { exponent : Int, significand : Float}
    content =
        { exponent = 1000, significand = 1.234 }


    getInternal (create content)
       == content --> True

-}
getInternal : WideFloat -> { exponent : Int, significand : Float }
getInternal (WideFloat w) =
    w


{-| Returns a `String` value representing the base 10 expression of a `WideFloat`. The precision is a bit lower than other calcurations implemented in this module.

    create
        { exponent = 2
        , significand = 1
        }
        |> toString --> "4e0"

-}
toString : WideFloat -> String
toString (WideFloat w) =
    let
        base10log =
            (logBase 4 (w.significand * w.significand)
                + toFloat w.exponent
            )
                * 0.3010299956639812

        base10exponent =
            floor base10log

        sgn =
            if w.significand < 0 then
                -1

            else
                1

        significand =
            10.0 ^ (base10log - toFloat base10exponent) * sgn
    in
    String.fromFloat significand
        ++ "e"
        ++ String.fromInt base10exponent
