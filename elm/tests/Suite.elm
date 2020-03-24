module Suite exposing (suite)

import Either
import Expect
import Json.Decode as D
import Main exposing (Task, taskDecoder)
import Test exposing (Test)


type alias T =
    { name : String
    , json : String
    , result : List Task
    }


tests : List T
tests =
    [ T "start" "[]" []
    ]


suite : Test
suite =
    let
        runTest : T -> Test
        runTest x =
            let
                testDecoder : Expect.Expectation
                testDecoder =
                    D.decodeString (D.list taskDecoder) x.json
                        |> Either.fromResult
                        |> Either.unpack
                            (D.errorToString >> Expect.fail)
                            (Expect.equal x.result)
            in
            Test.test x.name (\_ -> testDecoder)
    in
    List.map runTest tests |> Test.concat
