module Main exposing (..)

import AssocList as AL
import Browser
import Cmd.Extra exposing (withNoCmd)
import Http
import Json.Decode as JD
import List.Selection as LS
import Maybe.Extra as Maybe
import ReworkUI.Decoder
    exposing
        ( decodeFlags
        , decodeMonitor
        , decodeService
        , taskDecoder
        )
import ReworkUI.Type
    exposing
        ( Action(..)
        , Domain
        , DomainDict
        , Flags
        , IntDict
        , Model
        , Msg(..)
        , Service
        , ServiceDict
        , Status(..)
        , TableLayout(..)
        , Task
        , TaskDict
        , TaskResult(..)
        , User(..)
        , Worker
        , WorkerDict
        )
import ReworkUI.View exposing (view)
import Time
import Url.Builder as UB


selectModelModifier : TableLayout -> Int -> Action -> (Model -> Model)
selectModelModifier table id action =
    case table of
        TableTasks ->
            updateTaskActions action |> modifyIntDict id |> updateTask

        TableMonitors ->
            updateWorkerActions action |> modifyIntDict id |> updateWorker

        TableServices ->
            identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setActionModel : TableLayout -> Int -> Action -> Model
        setActionModel table id action =
            selectModelModifier table id action model
    in
    case msg of
        OnDelete taskId ->
            ( setActionModel TableTasks taskId (Pending Delete)
            , cmdGet
                (UB.crossOrigin
                    model.urlPrefix
                    [ "delete-task", String.fromInt taskId ]
                    []
                )
                (Http.expectJson (GotBool TableTasks taskId Delete) JD.bool)
            )

        OnAbort taskId ->
            ( setActionModel TableTasks taskId (Pending Abort)
            , cmdGet
                (UB.crossOrigin
                    model.urlPrefix
                    [ "abort-task", String.fromInt taskId ]
                    []
                )
                (Http.expectJson (GotBool TableTasks taskId Abort) JD.bool)
            )

        OnRelaunch taskId ->
            ( setActionModel TableTasks taskId (Pending Relaunch)
            , cmdPut
                (UB.crossOrigin
                    model.urlPrefix
                    [ "relaunch-task", String.fromInt taskId ]
                    []
                )
                (Http.expectJson (RelaunchMsg taskId) JD.int)
            )

        NoOperation ->
            ( model, Cmd.none )

        GotTasks (Ok tasks) ->
            ( setTask (AL.fromList (listTupleTask tasks)) model, Cmd.none )

        GotTasks (Err _) ->
            ( { model | errorMessage = Just "Could not load tasks" }, Cmd.none )

        OnRefresh ->
            ( model, refreshCmd model.urlPrefix model.tableLayout model.userDomain )

        GotBool table taskId action (Ok True) ->
            setActionModel table taskId (Completed action) |> withNoCmd

        GotBool table taskId action (Ok False) ->
            setActionModel table taskId (Uncompleted action) |> withNoCmd

        GotBool table taskId action (Err _) ->
            setActionModel table taskId (Uncompleted action) |> withNoCmd

        RelaunchMsg taskId (Ok 0) ->
            setActionModel TableTasks taskId (Uncompleted Relaunch) |> withNoCmd

        RelaunchMsg taskId (Ok _) ->
            setActionModel TableTasks taskId (Completed Relaunch) |> withNoCmd

        RelaunchMsg taskId (Err _) ->
            setActionModel TableTasks taskId (Uncompleted Relaunch) |> withNoCmd

        Table tableLayout ->
            ( { model | tableLayout = tableLayout }
            , refreshCmd model.urlPrefix tableLayout model.userDomain
            )

        GotServices (Ok services) ->
            ( setService (AL.fromList (listTupleService services)) model, Cmd.none )

        GotServices (Err _) ->
            ( { model | errorMessage = Just "Could not load services" }, Cmd.none )

        GotMonitors (Ok monitor) ->
            ( setMonitor
                (AL.fromList (listTupleDomain monitor.domains))
                (AL.fromList (listTupleWorker monitor.workers))
                model
            , Cmd.none
            )

        GotMonitors (Err _) ->
            ( { model | errorMessage = Just "Could not load monitors" }, Cmd.none )

        OnKill wId ->
            ( setActionModel TableMonitors wId (Pending Kill)
            , cmdGet
                (UB.crossOrigin
                    model.urlPrefix
                    [ "kill-worker", String.fromInt wId ]
                    []
                )
                (Http.expectJson (GotBool TableMonitors wId Kill) JD.bool)
            )

        OnShutdown wId ->
            ( setActionModel TableMonitors wId (Pending Shutdown)
            , cmdGet
                (UB.crossOrigin
                    model.urlPrefix
                    [ "Shutdown-worker", String.fromInt wId ]
                    []
                )
                (Http.expectJson (GotBool TableMonitors wId Shutdown) JD.bool)
            )

        SetDomain domain ->
            { model | userDomain = LS.select domain model.userDomain }
                |> withNoCmd


listTupleTask : List Task -> List ( Int, Task )
listTupleTask listtask =
    let
        creatTuple : Task -> ( Int, Task )
        creatTuple task =
            ( task.id, task )
    in
    List.map creatTuple listtask


listTupleService : List Service -> List ( Int, Service )
listTupleService listservice =
    let
        creatTuple : Service -> ( Int, Service )
        creatTuple service =
            ( service.opid, service )
    in
    List.map creatTuple listservice


listTupleDomain : List Domain -> List ( Int, Domain )
listTupleDomain listdomain =
    let
        creatTuple : Domain -> ( Int, Domain )
        creatTuple domain =
            ( domain.id, domain )
    in
    List.map creatTuple listdomain


listTupleWorker : List Worker -> List ( Int, Worker )
listTupleWorker listworker =
    let
        creatTuple : Worker -> ( Int, Worker )
        creatTuple worker =
            ( worker.wId, worker )
    in
    List.map creatTuple listworker


updateTaskActions : Action -> Task -> Task
updateTaskActions action task =
    { task | actions = [ action ] }


updateWorkerActions : Action -> Worker -> Worker
updateWorkerActions action worker =
    { worker | actions = [ action ] }


cmdGet : String -> Http.Expect msg -> Cmd msg
cmdGet url expect =
    Http.get
        { url = url
        , expect = expect
        }


cmdPut : String -> Http.Expect msg -> Cmd msg
cmdPut url expect =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


setTask : TaskDict -> Model -> Model
setTask task model =
    { model | task = task }


setService : ServiceDict -> Model -> Model
setService service model =
    { model | service = service }


setMonitor : DomainDict -> WorkerDict -> Model -> Model
setMonitor domain worker model =
    { model
        | domain = domain
        , worker = worker
    }


modifyIntDict : Int -> (a -> a) -> (IntDict a -> IntDict a)
modifyIntDict id modify intDict =
    let
        justUpate : Maybe a -> Maybe a
        justUpate a =
            Maybe.map modify a
    in
    AL.update id justUpate intDict


updateTask : (IntDict Task -> IntDict Task) -> (Model -> Model)
updateTask modify model =
    { model | task = modify model.task }


updateWorker : (IntDict Worker -> IntDict Worker) -> (Model -> Model)
updateWorker modify model =
    { model | worker = modify model.worker }


modifyTask : Int -> (Task -> Task) -> Model -> Model
modifyTask taskId modify model =
    let
        justUpate : Maybe Task -> Maybe Task
        justUpate task =
            Maybe.map modify task
    in
    setTask (AL.update taskId justUpate model.task) model


refreshCmd : String -> TableLayout -> LS.Selection String -> Cmd Msg
refreshCmd urlPrefix tableLayout userDomain =
    let
        ( urlPart, expect ) =
            case tableLayout of
                TableTasks ->
                    ( "tasks-table"
                    , Http.expectJson GotTasks (JD.list taskDecoder)
                    )

                TableServices ->
                    ( "services-table-json"
                    , Http.expectJson GotServices (JD.list decodeService)
                    )

                TableMonitors ->
                    ( "workers-table-json"
                    , Http.expectJson GotMonitors decodeMonitor
                    )

        query =
            LS.selected userDomain
                |> Maybe.map (UB.string "domain")
                |> Maybe.toList
    in
    cmdGet (UB.crossOrigin urlPrefix [ urlPart ] query) expect


init : JD.Value -> ( Model, Cmd Msg )
init jsonFlags =
    let
        { urlPrefix, domains } =
            case JD.decodeValue decodeFlags jsonFlags of
                Ok a ->
                    a

                Err _ ->
                    Flags "" []

        userDomain =
            Maybe.unwrap
                (LS.fromList domains)
                (\x -> LS.select x (LS.fromList domains))
                (List.head domains)
    in
    ( Model
        Nothing
        AL.empty
        AL.empty
        AL.empty
        AL.empty
        urlPrefix
        TableTasks
        userDomain
    , refreshCmd urlPrefix TableTasks userDomain
    )


sub : Model -> Sub Msg
sub model =
    let
        refreshTime =
            case model.tableLayout of
                TableTasks ->
                    1000

                TableServices ->
                    10000

                TableMonitors ->
                    2000
    in
    Time.every refreshTime (always OnRefresh)


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }
