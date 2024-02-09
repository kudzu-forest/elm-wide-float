module WideFloat_Test exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)
import Round
import Test exposing (..)
import WideFloat exposing (..)


suite : Test
suite =
    describe "WideFloat module"
        [ Test.fuzz
            (Fuzz.pair Fuzz.niceFloat Fuzz.niceFloat)
            "all in one"
            (\( a, b ) ->
                let
                    correctDict =
                        Dict.fromList
                            [ ( "added", a + b )
                            , ( "diff", a - b )
                            , ( "mult", a * b )
                            , ( "multf", a * b )
                            , ( "div", a / b )
                            , ( "proportion", a / (a + b) )
                            ]
                            |> Dict.map
                                (\_ f ->
                                    withDefault 3.1415926535 f
                                )

                    wa =
                        fromFloat a

                    wb =
                        fromFloat b

                    targetDict =
                        Dict.fromList
                            [ ( "added", add wa wb )
                            , ( "diff", wa |> differenceFrom wb )
                            , ( "mult", multiply wa wb )
                            , ( "multf", multiplyFloat b wa )
                            , ( "div", wa |> dividedBy wb )
                            ]
                            |> Dict.map
                                (\_ w ->
                                    let
                                        o =
                                            getInternal w
                                    in
                                    toFloat (2 ^ o.base2exponent)
                                        * o.significand
                                )
                            |> Dict.insert "proportion"
                                (proportionOf wa wb)
                            |> Dict.map
                                (\n f ->
                                    let
                                        c =
                                            Dict.get n correctDict
                                                |> Maybe.withDefault
                                                    2.718281828
                                    in
                                    withDefault c f
                                )
                in
                targetDict
                    |> Expect.equalDicts correctDict
            )
        ]


withDefault : Float -> Float -> Float
withDefault default input =
    if isNaN input || isInfinite input then
        default

    else
        input
