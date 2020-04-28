module Suite exposing (testParser)

import Expect
import Json.Decode as D
import ReworkUI.Decoder
    exposing
        ( decodeWorker
        , matchTaskResult
        , statusDecoder
        , taskDecoder
        , userDecoder
        , workerActionsDecoder
        )
import ReworkUI.Type
    exposing
        ( Action(..)
        , Status(..)
        , Task
        , TaskResult(..)
        , User(..)
        , Worker
        )
import Test as T


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
        UnknownUser
        14
        Done
        (Just "Got a TERMINATE/15")
        [ Relaunch, Delete ]


userInput : String
userInput =
    """
[   {},
    {"user" : "toto"},
    {"user" : "titi","options":{}},
    {"user" : "titi","options":{"run_name": "tutu"}},
    {"options":{"run_name": "tutu"}}
]
    """


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


taskResultDecoder : D.Decoder TaskResult
taskResultDecoder =
    let
        decodeTaskResult : Status -> D.Decoder TaskResult
        decodeTaskResult status =
            D.succeed (matchTaskResult status)
    in
    statusDecoder |> D.andThen decodeTaskResult


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


buttonsInput : String
buttonsInput =
    """
[
    {"kill" : false, "shutdown" : false},
    {"kill" : true , "shutdown" : true},
    {"kill" : true , "shutdown" : false},
    {"kill" : false, "shutdown" : true}
]
"""


inputWorker : String
inputWorker =
    """
{
  "button": {
      "kill": false,
      "shutdown": true
  },
  "cpu": 0,
  "debugPort": null,
  "domain": "default",
  "host": "51.15.183.93",
  "mem": 45,
  "pid": 966593,
  "started": "2020-04-27 17:49:57+0200",
  "wId": 142
}
"""


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
        , T.test "userDecoder"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list userDecoder) userInput)
                    (Ok
                        [ UnknownUser
                        , NamedUser "toto"
                        , NamedUser "titi"
                        , RunUser "titi" "tutu"
                        , UnknownUser
                        ]
                    )
            )
        , T.test "workerActionsDecoder"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list workerActionsDecoder) buttonsInput)
                    (Ok
                        [ [ Kill, Shutdown ]
                        , [ Pending Kill, Pending Shutdown ]
                        , [ Pending Kill, Shutdown ]
                        , [ Kill, Pending Shutdown ]
                        ]
                    )
            )
        , T.test "decodeWorker"
            (\_ ->
                Expect.equal
                    (D.decodeString decodeWorker inputWorker)
                    (Ok <|
                        Worker
                            142
                            "51.15.183.93"
                            966593
                            "default"
                            45
                            0
                            Nothing
                            "2020-04-27 17:49:57+0200"
                            [ Kill, Pending Shutdown ]
                    )
            )
        ]
