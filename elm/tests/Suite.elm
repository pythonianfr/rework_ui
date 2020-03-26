module Suite exposing (testParser)

import Expect
import Json.Decode as D
import Main exposing (Status(..), TaskResult(..))
import Test as T


type alias Task =
    { id : Int
    , name : String
    , domain : String
    , queued : String
    , started : String
    , finished : String
    , worker : Int
    , status : Status
    }


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
    "deathinfo": "Got a TERMINATE/15 signal while at <frame at 0x7f9640e74af8, file '/home/pythonian/src/rework/rework/monitor.py', line 523, code _run>",
    "traceback": null
  }
]
"""


taskHello : Task
taskHello =
    Task
        1
        "hello"
        "default"
        "2020-03-24 20:24:37+0100"
        "2020-03-24 20:24:37+0100"
        "2020-03-24 20:24:51+0100"
        14
        Done


taskDecoder : D.Decoder Task
taskDecoder =
    D.map8
        Task
        (D.field "tid" D.int)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.field "worker" D.int)
        statusDecoder


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


statusResult : List Status
statusResult =
    [ Queued
    , Aborting
    , Running
    , Aborting
    , Aborted
    , Failed "Python Traceback"
    , Done
    ]


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


taskResultDecoder : D.Decoder TaskResult
taskResultDecoder =
    let
        matchTaskResult : Status -> D.Decoder TaskResult
        matchTaskResult status =
            case status of
                Failed _ ->
                    D.succeed Failure

                _ ->
                    D.succeed Success
    in
    statusDecoder |> D.andThen matchTaskResult


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
                    (Ok statusResult)
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
        ]
