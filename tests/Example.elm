module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Round
import Test exposing (..)
import WideFloat exposing (..)


suite : Test
suite =
    describe "WideFloat module"
        [ Test.fuzz
            (Fuzz.pair Fuzz.float Fuzz.float
                |> Fuzz.listOfLength 10
            )
            "proportion"
            (\list ->
                let
                    correct =
                        list
                            |> List.map
                                (\( a, b ) -> a / (a + b))
                            |> List.map
                                (Round.round 8)

                    target =
                        list
                            |> List.map
                                (\( a, b ) ->
                                    proportion
                                        (fromFloat a)
                                        (fromFloat b)
                                )
                            |> List.map
                                (Round.round 8)
                in
                target
                    |> Expect.equal correct
            )
        ]
