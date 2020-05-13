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
        , TabsLayout(..)
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

adderror model error =
    { model | errors = List.append model.errors [error] }


unwraperror : Http.Error -> String
unwraperror resp =
    case resp of
        Http.BadUrl x -> "bad url: " ++ x
        Http.Timeout -> "the query timed out"
        Http.NetworkError -> "there was a network error"
        Http.BadStatus val -> "we got a bad status answer: " ++ String.fromInt val
        Http.BadBody body -> "we got a bad body: " ++ body


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setActionModel : TabsLayout -> Int -> Action -> Model
        setActionModel table id action =
            let
                updateactions actionable =
                    { actionable | actions = [ action ] }
            in
            case table of
                TasksTab ->
                    { model | tasks = AL.update id (Maybe.map updateactions) model.tasks }

                MonitorsTab ->
                    { model | workers = AL.update id (Maybe.map updateactions) model.workers }

                ServicesTab ->
                    model
    in
    case msg of
        OnDelete taskid ->
            ( setActionModel TasksTab taskid (Pending Delete)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "delete-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (GotBool TasksTab taskid Delete) JD.bool
                }
            )

        OnAbort taskid ->
            ( setActionModel TasksTab taskid (Pending Abort)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "abort-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (GotBool TasksTab taskid Abort) JD.bool
                }
            )

        OnRelaunch taskid ->
            ( setActionModel TasksTab taskid (Pending Relaunch)
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

        GotTasks (Ok rawtasks) ->
            case  JD.decodeString (JD.list taskDecoder) rawtasks of
                Ok tasks -> nocmd <| { model | tasks = AL.fromList (groupbyid tasks) }
                Err err -> nocmd <| adderror model <| JD.errorToString err

        GotTasks (Err err) ->
            nocmd <| adderror model <| unwraperror err

        OnRefresh ->
            ( model, refreshCmd model model.activetab )

        GotBool table taskid action (Ok True) ->
            nocmd <| setActionModel table taskid (Completed action)

        GotBool table taskid action (Ok False) ->
            nocmd <| setActionModel table taskid (Uncompleted action)

        GotBool table taskid action (Err _) ->
            nocmd <| setActionModel table taskid (Uncompleted action)

        RelaunchMsg taskid (Ok 0) ->
            nocmd <| setActionModel TasksTab taskid (Uncompleted Relaunch)

        RelaunchMsg taskid (Ok _) ->
            nocmd <| setActionModel TasksTab taskid (Completed Relaunch)

        RelaunchMsg taskid (Err _) ->
            nocmd <| setActionModel TasksTab taskid (Uncompleted Relaunch)

        Tab tab ->
            ( { model | activetab = tab }
            , refreshCmd model tab
            )

        GotServices (Ok services) ->
            nocmd <| { model | services = AL.fromList (groupbyid services) }

        GotServices (Err err) ->
            nocmd <| adderror model <| unwraperror err

        GotWorkers (Ok monitor) ->
            nocmd { model
                      | monitors = AL.fromList (groupbyid monitor.monitors)
                      , workers = AL.fromList (groupbyid monitor.workers)
                  }

        GotWorkers (Err err) ->
            nocmd <| adderror model <| unwraperror err

        OnKill wid ->
            ( setActionModel MonitorsTab wid (Pending Kill)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "kill-worker", String.fromInt wid ]
                        []
                , expect = Http.expectJson (GotBool MonitorsTab wid Kill) JD.bool
                }
            )

        OnShutdown wid ->
            ( setActionModel MonitorsTab wid (Pending Shutdown)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "shutdown-worker", String.fromInt wid ]
                        []
                , expect = Http.expectJson (GotBool MonitorsTab wid Shutdown) JD.bool
                }
            )

        SetDomain domain ->
            nocmd <| { model | domain = LS.select domain model.domain }


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


refreshCmd : Model -> TabsLayout -> Cmd Msg
refreshCmd model tab =
    let
        ( urlPart, expect ) =
            case tab of
                TasksTab ->
                    ( "tasks-table"
                    , Http.expectString GotTasks
                    )

                ServicesTab ->
                    ( "services-table-json"
                    , Http.expectJson GotServices (JD.list decodeService)
                    )

                MonitorsTab ->
                    ( "workers-table-json"
                    , Http.expectJson GotWorkers decodeWorkers
                    )

        query =
            LS.selected model.domain
                |> Maybe.map (UB.string "domain")
                |> Maybe.toList
    in
    Http.get
        { url = UB.crossOrigin model.urlPrefix [ urlPart ] query
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

        domain =
            Maybe.unwrap
                (LS.fromList domains)
                (\x -> LS.select x (LS.fromList domains))
                (List.head domains)

        model = Model
                []
                AL.empty
                AL.empty
                AL.empty
                AL.empty
                urlPrefix
                TasksTab
                domain
    in
    ( model
    , refreshCmd model TasksTab
    )


sub : Model -> Sub Msg
sub model =
    let
        refreshTime =
            case model.activetab of
                TasksTab ->
                    1000

                ServicesTab ->
                    10000

                MonitorsTab ->
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
