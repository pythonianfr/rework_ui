module ReworkUI.Decoder exposing
    ( decodeMonitor
    , decodeService
    , matchTaskResult
    , statusDecoder
    , taskDecoder
    , userDecoder
    )

import Json.Decode as D
import ReworkUI.Type
    exposing
        ( Action(..)
        , Button
        , Domain
        , JsonMonitors
        , JsonStatus
        , JsonUser
        , Msg(..)
        , Service
        , Status(..)
        , Task
        , TaskResult(..)
        , User(..)
        , Worker
        )


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
            [ Wait ]

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


matchUser : JsonUser -> User
matchUser x =
    case ( x.user, x.runName ) of
        ( Just u, Just r ) ->
            RunUser u r

        ( Just u, Nothing ) ->
            NamedUser u

        _ ->
            UnknownUser


userDecoder : D.Decoder User
userDecoder =
    let
        jsonUserDecoder : D.Decoder JsonUser
        jsonUserDecoder =
            D.map2 JsonUser
                (optionalAt [ "user" ] D.string)
                (optionalAt [ "options", "run_name" ] D.string)
    in
    jsonUserDecoder |> D.andThen (matchUser >> D.succeed)


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
        userDecoder
        (D.field "worker" D.int)
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


decodeDomain : D.Decoder Domain
decodeDomain =
    D.map5 Domain
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
        (D.field "button" decodeButton)


decodeButton : D.Decoder Button
decodeButton =
    D.map2 Button
        (D.field "kill" D.bool)
        (D.field "shutDown" D.bool)


decodeMonitor : D.Decoder JsonMonitors
decodeMonitor =
    D.map2 JsonMonitors
        (D.field "domains" (D.list decodeDomain))
        (D.field "workers" (D.list decodeWorker))
