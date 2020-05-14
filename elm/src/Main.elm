module Main exposing (..)

import AssocList as AL
import AssocList.Extra as ALE
import Browser
import Cmd.Extra exposing (withNoCmd)
import Http
import Json.Decode as JD
import List.Extra as LE
import List.Selection as LS
import Maybe.Extra as Maybe
import ReworkUI.Decoder
    exposing
        ( eventsdecoder
        , decodeFlags
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
        , Worker
        , WorkerDict
        )
import ReworkUI.View exposing (view)
import AssocSet as AS
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


handleevents model events =
    -- remove deleted events in place
    -- and query the tasks using min/max concerned ids
    let
        (alldeleted, allothers) = List.partition (\e -> e.action == "D") events
        deletedids = List.map .taskid alldeleted
        others = List.map .taskid allothers
        newmodel = { model
                       | tasks = ALE.removeMany (AS.fromList deletedids) model.tasks
                       , lasteventid = Maybe.withDefault
                                       model.lasteventid
                                       <| LE.foldl1 max <| List.map .id events
                   }
    in
    ( newmodel
    , if List.length others > 0
      then Http.get
          <| tasksquery model UpdatedTasks (LE.foldl1 min others) (LE.foldl1 max others)
      else Cmd.none
    )


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
                , expect = Http.expectJson (ActionResponse TasksTab taskid Delete) JD.bool
                }
            )

        OnAbort taskid ->
            ( setActionModel TasksTab taskid (Pending Abort)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "abort-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (ActionResponse TasksTab taskid Abort) JD.bool
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
                Ok tasks -> nocmd { model | tasks = AL.fromList (groupbyid tasks) }
                Err err -> nocmd <| adderror model <| JD.errorToString err

        GotTasks (Err err) ->
            nocmd <| adderror model <| unwraperror err

        UpdatedTasks (Ok rawtasks) ->
            case  JD.decodeString (JD.list taskDecoder) rawtasks of
                Ok tasks -> nocmd { model
                                      | tasks = AL.union
                                        (AL.fromList (groupbyid tasks))
                                        model.tasks
                                  }
                Err err -> nocmd <| adderror model <| JD.errorToString err

        UpdatedTasks (Err err) ->
            nocmd <| adderror model <| unwraperror err

        GotEvents (Ok rawevents) ->
            case JD.decodeString eventsdecoder rawevents of
                Err err -> nocmd <| adderror model <| JD.errorToString err
                Ok maybeevents ->
                    case maybeevents of
                        Nothing ->
                            nocmd model
                        Just events ->
                            -- try to update the model with minimal effort
                            handleevents model events

        GotEvents (Err err) ->
            nocmd <| adderror model <| unwraperror err

        GotLastEvent (Ok rawid) ->
            case JD.decodeString JD.int rawid of
                Ok evid -> nocmd { model | lasteventid = evid }
                Err err -> nocmd <| adderror model <| JD.errorToString err

        GotLastEvent (Err err) ->
            nocmd <| adderror model <| unwraperror err

        OnRefresh ->
            ( model, refreshCmd model model.activetab )

        ActionResponse _ _ _ (Ok True) ->
            nocmd <| model

        ActionResponse table id action (Ok False) ->
            nocmd <| setActionModel table id (Uncompleted action)

        ActionResponse table id action (Err _) ->
            nocmd <| setActionModel table id (Uncompleted action)

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
                , expect = Http.expectJson (ActionResponse MonitorsTab wid Kill) JD.bool
                }
            )

        OnShutdown wid ->
            ( setActionModel MonitorsTab wid (Pending Shutdown)
            , Http.get
                { url = UB.crossOrigin
                        model.urlPrefix
                        [ "shutdown-worker", String.fromInt wid ]
                        []
                , expect = Http.expectJson (ActionResponse MonitorsTab wid Shutdown) JD.bool
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


eventsquery model =
    { url = UB.crossOrigin model.urlPrefix
          [ "events", String.fromInt model.lasteventid ]
          []
    , expect = Http.expectString GotEvents
    }


lasteventquery model =
    { url = UB.crossOrigin model.urlPrefix
          [ "lasteventid" ]
          []
    , expect = Http.expectString GotLastEvent
    }


tasksquery model msg min max =
    let
        args = [] ++
               (case min of
                   Nothing -> []
                   Just num -> [ UB.int "min" num ])
               ++
               (case max of
                   Nothing -> []
                   Just num -> [ UB.int "max" num ])
    in
    { url = UB.crossOrigin model.urlPrefix
          [ "tasks-table-json" ]
          args
    , expect = Http.expectString msg
    }


refreshCmd : Model -> TabsLayout -> Cmd Msg
refreshCmd model tab =
    let
        domain =
            LS.selected model.domain
                |> Maybe.map (UB.string "domain")
                |> Maybe.toList

        query =
            case tab of
                TasksTab ->
                    eventsquery model
                ServicesTab ->
                    { url = UB.crossOrigin model.urlPrefix
                            [ "services-table-json" ] [ ]
                    , expect = Http.expectJson GotServices (JD.list decodeService)
                    }

                MonitorsTab ->
                    { url = UB.crossOrigin model.urlPrefix
                            [ "workers-table-json" ] [ ]
                    , expect = Http.expectJson GotWorkers decodeWorkers
                    }

    in
    Http.get query


init : JD.Value -> ( Model, Cmd Msg )
init jsonFlags =
    let
        { urlPrefix, domains } =
            case JD.decodeValue decodeFlags jsonFlags of
                Ok val ->
                    let
                        dom =
                            if List.length val.domains > 1 then
                                "all" :: val.domains
                            else val.domains
                    in
                    Flags val.urlPrefix dom

                Err _ ->
                    Flags "" [ "default" ]

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
                0
    in
    ( model
    , Cmd.batch [ Http.get <| tasksquery model GotTasks Nothing Nothing
                , Http.get <| lasteventquery model
                ]
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
