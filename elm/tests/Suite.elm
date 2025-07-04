module Suite exposing (testParser)

import Expect
import Json.Decode as D
import Decoder
    exposing
        ( decodeworkers
        , matchactionresult
        , decodetask
        , decodeworkeraction
        , decodestatus
        , matchtaskresult
        , decodeworker
        )
import Type
    exposing
        ( Action(..)
        , Status(..)
        , Task
        , TaskResult(..)
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
    "traceback": null,
    "input": null
  }
]
"""


taskHello : Task
taskHello =
    { id = 1
    , result = Success
    , name = "hello"
    , domain = "default"
    , queued = "2020-03-24 20:24:37+0100"
    , started = Just "2020-03-24 20:24:37+0100"
    , finished = Just "2020-03-24 20:24:51+0100"
    , metadata = Nothing
    , worker = Just 14
    , status = Done
    , deathInfo = Just "Got a TERMINATE/15"
    , actions = [ Relaunch, Delete ]
    , input = Nothing
    }


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
            D.succeed (matchtaskresult status)
    in
    decodestatus |> D.andThen decodeTaskResult


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
  "debugport": null,
  "domain": "default",
  "host": "51.15.183.93",
  "mem": 45,
  "pid": 966593,
  "started": "2020-04-27 17:49:57+0200",
  "wid": 142
}
"""


testParser : T.Test
testParser =
    T.describe "testParser"
        [ T.test "taskHello"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list decodetask) inputHello)
                    (Ok [ taskHello ])
            )
        , T.test "statusParser"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list decodestatus) statusInput)
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
                    (D.decodeString decodestatus statusFailure
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
        , T.test "workerActionsDecoder"
            (\_ ->
                Expect.equal
                    (D.decodeString (D.list decodeworkeraction) buttonsInput)
                    (Ok
                        [ [ Kill, Shutdown ]
                        , [ Disabled Kill, Disabled Shutdown ]
                        , [ Disabled Kill, Shutdown ]
                        , [ Kill, Disabled Shutdown ]
                        ]
                    )
            )
        , T.test "decodeWorker"
            (\_ ->
                Expect.equal
                    (D.decodeString decodeworker inputWorker)
                    (Ok <|
                        { id = 142
                        , host = "51.15.183.93"
                        , pid = 966593
                        , domain = "default"
                        , mem = 45
                        , cpu = 0
                        , debugPort = Nothing
                        , started = "2020-04-27 17:49:57+0200"
                        , actions = [ Kill, Disabled Shutdown ]
                        }
                    )
            )
        ]
