module Decoder exposing
    ( decodeflags
    , decodeinputspec
    , decodelauncher
    , decodeplan
    , decodeworkers
    , decodescheduler
    , decodeservice
    , decodeevents
    , matchactionresult
    , decodetask
    , decodeworkeraction
    )

import Json.Decode as D
import Json.Decode.Field as F
import Metadata as M
import Type
    exposing
        ( Action(..)
        , Monitor
        , Event
        , Flags
        , IOSpec
        , JsonMonitors
        , JsonStatus
        , Launcher
        , Msg(..)
        , Plan
        , Scheduler
        , Service
        , SpecType(..)
        , Status(..)
        , Task
        , TaskResult(..)
        , Worker
        )


decodeevent =
    D.map3 Event
        (D.field "id" D.int)
        (D.field "action" D.string)
        (D.field "taskid" D.int)


decodeevents =
    D.nullable (D.list decodeevent)


decodestatus : D.Decoder Status
decodestatus =
    let
        decodejsonstatus : D.Decoder JsonStatus
        decodejsonstatus =
            D.map3 JsonStatus
                (D.field "status" D.string)
                (D.field "abort" D.bool)
                (D.field "traceback" (D.nullable D.string))

        matchstatus : JsonStatus -> D.Decoder Status
        matchstatus x =
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
    decodejsonstatus |> D.andThen matchstatus


matchtaskresult : Status -> TaskResult
matchtaskresult status =
    case status of
        Failed _ ->
            Failure

        _ ->
            Success


matchactionresult : Status -> List Action
matchactionresult status =
    case status of
        Running ->
            [ Abort ]

        Aborting ->
            [ Disabled Abort ]

        Done ->
            [ Relaunch, Delete ]

        _ ->
            [ Relaunch, Delete ]


map13 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> value)
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
    -> D.Decoder m
    -> D.Decoder value
map13 func da db dc dd de df dg dh di dj dk dl dm =
    let
        map5 : (i -> j -> k -> l -> m -> value) -> D.Decoder value
        map5 funcIJKLM =
            D.map5 funcIJKLM di dj dk dl dm
    in
    D.map8 func da db dc dd de df dg dh |> D.andThen map5


decodetaskfromstatus : Status -> D.Decoder Task
decodetaskfromstatus status =
    map13
        Task
        (D.field "tid" D.int)
        (D.succeed <| matchtaskresult status)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" (D.maybe D.string))
        (D.field "finished" (D.maybe D.string))
        (D.field "metadata" (D.maybe (D.dict M.metavaldecoder)))
        (D.field "worker" (D.maybe D.int))
        (D.succeed status)
        (D.field "deathinfo" (D.nullable D.string))
        (D.succeed <| matchactionresult status)
        (D.field "input" (D.nullable D.string))


decodetask : D.Decoder Task
decodetask =
    decodestatus |> D.andThen decodetaskfromstatus


decodeservice : D.Decoder Service
decodeservice =
    D.map5 Service
        (D.field "opid" D.int)
        (D.field "host" D.string)
        (D.field "name" D.string)
        (D.field "path" D.string)
        (D.field "domain" D.string)


decodeinputspec : D.Decoder IOSpec
decodeinputspec =
    F.require "type" D.string <| \stype ->
    F.require "name" D.string <| \name ->
    F.require "required" D.bool <| \required ->
    F.require "default" (D.nullable D.string) <| \default ->
    F.require "choices" (D.nullable (D.list D.string)) <| \choices ->

    D.succeed
        { spectype = case stype of
                         "file" -> File
                         "string" -> Str
                         "number" -> Num
                         "boolean" -> Bool
                         "datetime" -> Datetime
                         "moment" -> Moment
                         _ -> Str
        , name = name
        , required = required
        , default = default
        , choices = choices
        }


decodelauncher : D.Decoder Launcher
decodelauncher =
    D.map5 Launcher
        (D.index 0 D.int)
        (D.index 1 D.string)
        (D.index 2 D.string)
        (D.index 3 D.string)
        (D.index 4 (D.list decodeinputspec))


decodescheduler : D.Decoder Scheduler
decodescheduler =
    D.map6 Scheduler
        (D.index 0 D.int)
        (D.index 1 D.string)
        (D.index 2 D.string)
        (D.index 3 D.string)
        (D.index 4 D.string)
        (D.index 5 (D.nullable D.string))


decodeplan : D.Decoder Plan
decodeplan =
    D.map5 Plan
        (D.index 0 D.int)
        (D.index 1 D.string)
        (D.index 2 D.string)
        (D.index 3 D.string)
        (D.index 4 D.string)


decodemonitor : D.Decoder Monitor
decodemonitor =
    D.map5 Monitor
        (D.field "id" D.int)
        (D.field "domain" D.string)
        (D.field "delta" D.float)
        (D.field "lastseen" D.string)
        (D.field "options" (D.list decodeoption))


decodeoption : D.Decoder ( String, Int )
decodeoption =
    D.map2 Tuple.pair
        (D.index 0 D.string)
        (D.index 1 D.int)


map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder value
map9 func da db dc dd de df dg dh di =
    let
        map : (i -> value) -> D.Decoder value
        map funcI =
            D.map funcI di
    in
    D.map8 func da db dc dd de df dg dh |> D.andThen map


decodeworker : D.Decoder Worker
decodeworker =
    map9 Worker
        (D.field "wid" D.int)
        (D.field "host" D.string)
        (D.field "pid" D.int)
        (D.field "domain" D.string)
        (D.field "mem" D.int)
        (D.field "cpu" D.float)
        (D.field "debugport" (D.nullable D.int))
        (D.field "started" D.string)
        (D.field "button" decodeworkeraction)


decodeworkers : D.Decoder JsonMonitors
decodeworkers =
    D.map2 JsonMonitors
        (D.field "domains" (D.list decodemonitor))
        (D.field "workers" (D.list decodeworker))


type alias JsonButton =
    { kill : Bool
    , shutdown : Bool
    }


decodejsonbutton : D.Decoder JsonButton
decodejsonbutton =
    D.map2 JsonButton
        (D.field "kill" D.bool)
        (D.field "shutdown" D.bool)


decodeworkeraction : D.Decoder (List Action)
decodeworkeraction =
    let
        checkPending : ( Bool, Action ) -> Action
        checkPending ( asked, action ) =
            if asked then
                Disabled action

            else
                action

        toWorkerActions : JsonButton -> List Action
        toWorkerActions { kill, shutdown } =
            List.map checkPending [ ( kill, Kill ), ( shutdown, Shutdown ) ]
    in
    decodejsonbutton |> D.andThen (toWorkerActions >> D.succeed)


decodeflags : D.Decoder Flags
decodeflags =
    D.map2 Flags
        (D.field "baseurl" D.string)
        (D.field "domains" (D.list D.string))
