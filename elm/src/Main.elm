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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setActionModel : TableLayout -> Int -> Action -> Model
        setActionModel table id action =
            let
                updateactions actionable =
                    { actionable | actions = [ action ] }
            in
            case table of
                TableTasks ->
                    { model | tasks = AL.update id (Maybe.map updateactions) model.tasks }

                TableMonitors ->
                    { model | workers = AL.update id (Maybe.map updateactions) model.workers }

                TableServices ->
                    model
    in
    case msg of
        OnDelete taskid ->
            ( setActionModel TableTasks taskid (Pending Delete)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "delete-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (GotBool TableTasks taskid Delete) JD.bool
                }
            )

        OnAbort taskid ->
            ( setActionModel TableTasks taskid (Pending Abort)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "abort-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (GotBool TableTasks taskid Abort) JD.bool
                }
            )

        OnRelaunch taskid ->
            ( setActionModel TableTasks taskid (Pending Relaunch)
            , cmdPut
                (UB.crossOrigin
                    model.urlPrefix
                    [ "relaunch-task", String.fromInt taskid ]
                    []
                )
                (Http.expectJson (RelaunchMsg taskid) JD.int)
            )

        NoOperation ->
            nocmd model

        GotTasks (Ok tasks) ->
            nocmd <| { model | tasks = AL.fromList (groupbyid tasks) }

        GotTasks (Err _) ->
            nocmd { model | errorMessage = Just "Could not load tasks" }

        OnRefresh ->
            ( model, refreshCmd model.urlPrefix model.tableLayout model.userDomain )

        GotBool table taskid action (Ok True) ->
            nocmd <| setActionModel table taskid (Completed action)

        GotBool table taskid action (Ok False) ->
            nocmd <| setActionModel table taskid (Uncompleted action)

        GotBool table taskid action (Err _) ->
            nocmd <| setActionModel table taskid (Uncompleted action)

        RelaunchMsg taskid (Ok 0) ->
            nocmd <| setActionModel TableTasks taskid (Uncompleted Relaunch)

        RelaunchMsg taskid (Ok _) ->
            nocmd <| setActionModel TableTasks taskid (Completed Relaunch)

        RelaunchMsg taskid (Err _) ->
            nocmd <| setActionModel TableTasks taskid (Uncompleted Relaunch)

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
