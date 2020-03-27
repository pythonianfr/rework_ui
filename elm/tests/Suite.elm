module Suite exposing (testParser)

import Expect
import Json.Decode as D
import Main exposing (Action(..), Status(..), Task, TaskResult(..))
import Test as T


map12 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder j
    -> D.Decoder k
    -> D.Decoder l
    -> D.Decoder value
map12 func da db dc dd de df dg dh di dj dk dl =
    let
        map4 : (i -> j -> k -> l -> value) -> D.Decoder value
        map4 funcIJKL =
            D.map4 funcIJKL di dj dk dl
    in
    D.map8 func da db dc dd de df dg dh |> D.andThen map4


inputHello : String
inputHello =
    """
[
  {
    "tid": 1,
    "name": "hello",
    "status": "done",
    "abort": false,
    "domain": "default",
    "operation": 1,
    "queued": "2020-03-24 20:24:37+0100",
    "started": "2020-03-24 20:24:37+0100",
    "finished": "2020-03-24 20:24:51+0100",
    "metadata": null,
    "worker": 14,
    "deathinfo": "Got a TERMINATE/15",
    "traceback": null
  }
]
"""


taskHello : Task
taskHello =
    Task
        1
        Success
        "hello"
        "default"
        "2020-03-24 20:24:37+0100"
        "2020-03-24 20:24:37+0100"
        "2020-03-24 20:24:51+0100"
        ""
        14
        Done
        (Just "Got a TERMINATE/15")
        [ Relaunch, Delete ]


taskDecoder : D.Decoder Task
taskDecoder =
    statusDecoder |> D.andThen decodeTask


decodeTask : Status -> D.Decoder Task
decodeTask status =
    map12
        Task
        (D.field "tid" D.int)
        (D.succeed <| matchTaskResult status)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.succeed "")
        (D.field "worker" D.int)
        (D.succeed status)
        (D.field "deathinfo" (D.nullable D.string))
        (D.succeed <| matchActionResult status)


statusInput : String
statusInput =
    """
[
    {
        "_result": "Queued",
        "status": "queued",
        "abort": false,
        "traceback": null
    },
    {
        "_result": "Aborting",
        "status": "queued",
        "abort": true,
        "traceback": null
    },
    {
        "_result": "Running",
        "status": "running",
        "abort": false,
        "traceback": null
    },
    {
        "_result": "Aborting",
        "status": "running",
        "abort": true,
        "traceback": null
    },
    {
        "_result": "Aborted",
        "status": "done",
        "abort": true,
        "traceback": null
    },
    {
        "_result": "Failed traceback",
        "status": "done",
        "abort": false,
        "traceback": "Python Traceback"
    },
    {
        "_result": "Done",
        "status": "done",
        "abort": false,
        "traceback": null
    }
]
"""


statusFailure : String
statusFailure =
    """
    {
        "_result": "Queued",
        "status": "unknown",
        "abort": false,
        "traceback": null
    }
    """


type alias JsonStatus =
    { status : String
    , abort : Bool
    , traceback : Maybe String
    }


statusDecoder : D.Decoder Status
statusDecoder =
    let
        jsonStatusDecoder : D.Decoder JsonStatus
        jsonStatusDecoder =
            D.map3 JsonStatus
                (D.field "status" D.string)
                (D.field "abort" D.bool)
                (D.field "traceback" (D.nullable D.string))

        matchStatus : JsonStatus -> D.Decoder Status
        matchStatus x =
            case ( x.status, x.abort, x.traceback ) of
                ( "queued", False, _ ) ->
                    D.succeed Queued

                ( "queued", True, _ ) ->
                    D.succeed Aborting

                ( "running", False, _ ) ->
                    D.succeed Running

                ( "running", True, _ ) ->
                    D.succeed Aborting

                ( "done", True, _ ) ->
                    D.succeed Aborted

                ( "done", False, Just traceback ) ->
                    D.succeed (Failed traceback)

                ( "done", False, Nothing ) ->
                    D.succeed Done

                ( status, _, _ ) ->
                    D.fail <| "Unknown status : " ++ status
    in
    jsonStatusDecoder |> D.andThen matchStatus


matchActionResult : Status -> List Action
matchActionResult status =
    case status of
        Running ->
            [ Abort ]

        Aborting ->
            [ Wait ]

        Done ->
            [ Relaunch, Delete ]

        _ ->
            [ Delete ]


matchTaskResult : Status -> TaskResult
matchTaskResult status =
    case status of
        Failed _ ->
            Failure

        _ ->
            Success


taskResultDecoder : D.Decoder TaskResult
taskResultDecoder =
    let
        decodeTaskResult : Status -> D.Decoder TaskResult
        decodeTaskResult status =
            D.succeed (matchTaskResult status)
    in
    statusDecoder |> D.andThen decodeTaskResult


testParser : T.Test
testParser =
    T.describe "testParser"
        [ T.test "taskHello"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list taskDecoder) inputHello)
                    (Ok [ taskHello ])
            )
        , T.test "statusParser"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list statusDecoder) statusInput)
                    (Ok
                        [ Queued
                        , Aborting
                        , Running
                        , Aborting
                        , Aborted
                        , Failed "Python Traceback"
                        , Done
                        ]
                    )
            )
        , T.test "statusParser failure"
            (\_ ->
                Expect.equal
                    (D.decodeString statusDecoder statusFailure
                        |> Result.mapError D.errorToString
                        |> Result.mapError (String.contains "Unknown status :")
                    )
                    (Err True)
            )
        , T.test "taskResult"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list taskResultDecoder) statusInput)
                    (Ok
                        [ Success
                        , Success
                        , Success
                        , Success
                        , Success
                        , Failure
                        , Success
                        ]
                    )
            )
        ]
