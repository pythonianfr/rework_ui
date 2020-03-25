module Suite exposing (testParser)

import Expect
import Json.Decode as D
import Test as T


type alias Task =
    { id : Int
    , name : String
    , domain : String
    , queued : String
    , started : String
    , finished : String
    , worker : Int
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


taskDecoder : D.Decoder Task
taskDecoder =
    D.map7
        Task
        (D.field "tid" D.int)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.field "worker" D.int)


testParser : T.Test
testParser =
    let
        testDecoder : String -> Result D.Error (List Task)
        testDecoder jsonString =
            D.decodeString (D.list taskDecoder) jsonString
    in
    T.describe "testParser"
        [ T.test "taskHello"
            (\_ ->
                Expect.equal
                    (testDecoder inputHello)
                    (Ok [ taskHello ])
            )
        ]
