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
        , decodeWorkers
        , decodeService
        , taskDecoder
        )
import ReworkUI.Type
    exposing
        ( Action(..)
        , Monitor
        , MonitorDict
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


nocmd = withNoCmd


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
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "delete-task", String.fromInt taskId ]
                        []
                , expect = Http.expectJson (GotBool TableTasks taskId Delete) JD.bool
                }
            )

        OnAbort taskId ->
            ( setActionModel TableTasks taskId (Pending Abort)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "abort-task", String.fromInt taskId ]
                        []
                , expect = Http.expectJson (GotBool TableTasks taskId Abort) JD.bool
                }
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
            nocmd model

        GotTasks (Ok tasks) ->
            nocmd <| { model | tasks = AL.fromList (groupbyid tasks) }

        GotTasks (Err _) ->
            nocmd { model | errorMessage = Just "Could not load tasks" }

        OnRefresh ->
            ( model, refreshCmd model.urlPrefix model.tableLayout model.userDomain )

        GotBool table taskId action (Ok True) ->
            nocmd <| setActionModel table taskId (Completed action)

        GotBool table taskId action (Ok False) ->
            nocmd <| setActionModel table taskId (Uncompleted action)

        GotBool table taskId action (Err _) ->
            nocmd <| setActionModel table taskId (Uncompleted action)

        RelaunchMsg taskId (Ok 0) ->
            nocmd <| setActionModel TableTasks taskId (Uncompleted Relaunch)

        RelaunchMsg taskId (Ok _) ->
            nocmd <| setActionModel TableTasks taskId (Completed Relaunch)

        RelaunchMsg taskId (Err _) ->
            nocmd <| setActionModel TableTasks taskId (Uncompleted Relaunch)

        Table tableLayout ->
            ( { model | tableLayout = tableLayout }
            , refreshCmd model.urlPrefix tableLayout model.userDomain
            )

        GotServices (Ok services) ->
            nocmd <| { model | services = AL.fromList (groupbyid services) }

        GotServices (Err _) ->
            nocmd { model | errorMessage = Just "Could not load services" }

        GotWorkers (Ok monitor) ->
            nocmd { model
                      | monitors = AL.fromList (groupbyid monitor.monitors)
                      , workers = AL.fromList (groupbyid monitor.workers)
                  }

        GotWorkers (Err _) ->
            nocmd { model | errorMessage = Just "Could not load monitors" }

        OnKill wId ->
            ( setActionModel TableMonitors wId (Pending Kill)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "kill-worker", String.fromInt wId ]
                        []
                , expect = Http.expectJson (GotBool TableMonitors wId Kill) JD.bool
                }
            )

        OnShutdown wId ->
            ( setActionModel TableMonitors wId (Pending Shutdown)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "shutdown-worker", String.fromInt wId ]
                        []
                , expect = Http.expectJson (GotBool TableMonitors wId Shutdown) JD.bool
                }
            )

        SetDomain domain ->
            nocmd <| { model | userDomain = LS.select domain model.userDomain }


groupbyid items =
    List.map (\item -> (item.id, item)) items


updateTaskActions : Action -> Task -> Task
updateTaskActions action task =
    { task | actions = [ action ] }


updateWorkerActions : Action -> Worker -> Worker
updateWorkerActions action worker =
    { worker | actions = [ action ] }


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


modifyIntDict : Int -> (a -> a) -> (IntDict a -> IntDict a)
modifyIntDict id modify intDict =
    AL.update id (Maybe.map modify) intDict


updateTask : (IntDict Task -> IntDict Task) -> (Model -> Model)
updateTask modify model =
    { model | tasks = modify model.tasks }


updateWorker : (IntDict Worker -> IntDict Worker) -> (Model -> Model)
updateWorker modify model =
    { model | workers = modify model.workers }


modifyTask : Int -> (Task -> Task) -> Model -> Model
modifyTask taskId modify model =
    let
        justUpate : Maybe Task -> Maybe Task
        justUpate task =
            Maybe.map modify task
    in
    { model | tasks = AL.update taskId justUpate model.tasks }


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
                    , Http.expectJson GotWorkers decodeWorkers
                    )

        query =
            LS.selected userDomain
                |> Maybe.map (UB.string "domain")
                |> Maybe.toList
    in
    Http.get
        { url = UB.crossOrigin urlPrefix [ urlPart ] query
        , expect = expect
        }


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
