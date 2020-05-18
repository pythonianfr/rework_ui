module ReworkUI.Decoder exposing
    ( decodeFlags
    , decodeWorkers
    , decodeService
    , decodeWorker
    , eventsdecoder
    , matchActionResult
    , matchTaskResult
    , statusDecoder
    , taskDecoder
    , workerActionsDecoder
    )

import Json.Decode as D
import ReworkUI.Metadata as M
import ReworkUI.Type
    exposing
        ( Action(..)
        , Monitor
        , Event
        , Flags
        , JsonMonitors
        , JsonStatus
        , Msg(..)
        , Service
        , Status(..)
        , Task
        , TaskResult(..)
        , Worker
        )


eventdecoder =
    D.map3 Event
        (D.field "id" D.int)
        (D.field "action" D.string)
        (D.field "taskid" D.int)


eventsdecoder =
    D.nullable (D.list eventdecoder)


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


matchTaskResult : Status -> TaskResult
matchTaskResult status =
    case status of
        Failed _ ->
            Failure

        _ ->
            Success


matchActionResult : Status -> List Action
matchActionResult status =
    case status of
        Running ->
            [ Abort ]

        Aborting ->
            [ Pending Abort ]

        Done ->
            [ Relaunch, Delete ]

        _ ->
            [ Relaunch, Delete ]


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


optionalAt : List String -> D.Decoder a -> D.Decoder (Maybe a)
optionalAt path da =
    D.oneOf [ D.at path (D.nullable da), D.succeed Nothing ]


decodeTask : Status -> D.Decoder Task
decodeTask status =
    map12
        Task
        (D.field "tid" D.int)
        (D.succeed <| matchTaskResult status)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" (D.maybe D.string))
        (D.field "finished" (D.maybe D.string))
        (D.field "metadata" (D.maybe (D.dict M.metavaldecoder)))
        (D.field "worker" (D.maybe D.int))
        (D.succeed status)
        (D.field "deathinfo" (D.nullable D.string))
        (D.succeed <| matchActionResult status)


taskDecoder : D.Decoder Task
taskDecoder =
    statusDecoder |> D.andThen decodeTask


decodeService : D.Decoder Service
decodeService =
    D.map5 Service
        (D.field "opid" D.int)
        (D.field "host" D.string)
        (D.field "name" D.string)
        (D.field "path" D.string)
        (D.field "domain" D.string)


decodeMonitor : D.Decoder Monitor
decodeMonitor =
    D.map5 Monitor
        (D.field "id" D.int)
        (D.field "domain" D.string)
        (D.field "delta" D.float)
        (D.field "lastSeen" D.string)
        (D.field "options" (D.list decodeOption))


decodeOption : D.Decoder ( String, Int )
decodeOption =
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


decodeWorker : D.Decoder Worker
decodeWorker =
    map9 Worker
        (D.field "wId" D.int)
        (D.field "host" D.string)
        (D.field "pid" D.int)
        (D.field "domain" D.string)
        (D.field "mem" D.int)
        (D.field "cpu" D.float)
        (D.field "debugPort" (D.nullable D.int))
        (D.field "started" D.string)
        (D.field "button" workerActionsDecoder)


decodeWorkers : D.Decoder JsonMonitors
decodeWorkers =
    D.map2 JsonMonitors
        (D.field "domains" (D.list decodeMonitor))
        (D.field "workers" (D.list decodeWorker))


type alias JsonButton =
    { kill : Bool
    , shutdown : Bool
    }


decodeJsonButton : D.Decoder JsonButton
decodeJsonButton =
    D.map2 JsonButton
        (D.field "kill" D.bool)
        (D.field "shutdown" D.bool)


workerActionsDecoder : D.Decoder (List Action)
workerActionsDecoder =
    let
        checkPending : ( Bool, Action ) -> Action
        checkPending ( asked, action ) =
            if asked then
                Pending action

            else
                action

        toWorkerActions : JsonButton -> List Action
        toWorkerActions { kill, shutdown } =
            List.map checkPending [ ( kill, Kill ), ( shutdown, Shutdown ) ]
    in
    decodeJsonButton |> D.andThen (toWorkerActions >> D.succeed)


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map2 Flags
        (D.field "urlPrefix" D.string)
        (D.field "domains" (D.list D.string))
