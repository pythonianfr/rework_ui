port module Main exposing (..)

import AssocList as AL
import AssocList.Extra as ALE
import Browser
import Browser.Events exposing (onKeyDown)
import Cmd.Extra exposing (withNoCmd)
import Http
import Json.Decode as JD
import Keyboard.Event exposing
    (KeyboardEvent
    , decodeKeyboardEvent
    )
import List.Extra as LE
import List.Selection as LS
import Log exposing
    ( Level(..)
    , log
    )
import Maybe.Extra as Maybe
import Decoder
    exposing
        ( eventsdecoder
        , decodeFlags
        , decodeLauncher
        , decodeScheduler
        , decodeWorkers
        , decodeService
        , matchActionResult
        , taskDecoder
        )
import Type
    exposing
        ( Action(..)
        , Flags
        , Model
        , Msg(..)
        , TabsLayout(..)
        )
import View exposing (view)
import AssocSet as AS
import Time
import Url.Builder as UB


nocmd = withNoCmd


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
        disableactions mod table id =
            let
                updateactions actionable =
                    { actionable |
                          actions = List.map (\action -> Disabled action) actionable.actions
                    }
            in
            case table of
                TasksTab ->
                    { mod | tasks = AL.update id (Maybe.map updateactions) mod.tasks }

                MonitorsTab ->
                    { mod | workers = AL.update id (Maybe.map updateactions) mod.workers }

                ServicesTab ->
                    mod

                LauncherTab ->
                    mod

                SchedulerTab ->
                    mod
    in
    case msg of
        OnDelete taskid ->
            ( disableactions
                  (log model INFO <| "DELETE " ++ String.fromInt taskid)
                  TasksTab taskid
            , Http.get
                { url = UB.crossOrigin
                        model.baseurl
                        [ "delete-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (ActionResponse TasksTab taskid Delete) JD.bool
                }
            )

        OnAbort taskid ->
            ( disableactions model TasksTab taskid
            , Http.get
                { url = UB.crossOrigin
                        model.baseurl
                        [ "abort-task", String.fromInt taskid ]
                        []
                , expect = Http.expectJson (ActionResponse TasksTab taskid Abort) JD.bool
                }
            )

        OnRelaunch taskid ->
            ( disableactions model TasksTab taskid
            , cmdPut
                (UB.crossOrigin
                    model.baseurl
                    [ "relaunch-task", String.fromInt taskid ]
                    []
                )
                (Http.expectJson (RelaunchMsg taskid) JD.int)
            )

        GotTasks (Ok rawtasks) ->
            let mod = log model INFO ("TASKS (all):" ++ rawtasks) in
            case  JD.decodeString (JD.list taskDecoder) rawtasks of
                Ok tasks -> nocmd { model | tasks = AL.fromList (groupbyid tasks) }
                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        GotTasks (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        UpdatedTasks (Ok rawtasks) ->
            let mod = log model INFO ("TASKS (subset):" ++ rawtasks) in
            case JD.decodeString (JD.list taskDecoder) rawtasks of
                Ok tasks -> nocmd { model
                                      | tasks = AL.union
                                        (AL.fromList (groupbyid tasks))
                                        model.tasks
                                  }
                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        UpdatedTasks (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotEvents (Ok rawevents) ->
            let mod =
                    if rawevents /= "[]"
                    then log model INFO ("EVENTS: " ++ rawevents)
                    else model
            in
            case JD.decodeString eventsdecoder rawevents of
                Err err -> nocmd <| log model ERROR <| JD.errorToString err
                Ok maybeevents ->
                    case maybeevents of
                        Nothing ->
                            -- resync with the server state
                            ( mod
                            , Cmd.batch
                                [ Http.get <| tasksquery model GotTasks Nothing Nothing
                                , Http.get <| lasteventquery model
                                ]
                            )
                        Just events ->
                            -- try to update the model with minimal effort
                            handleevents mod events

        GotEvents (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotLastEvent (Ok rawid) ->
            let mod = log model INFO ("LASTEVENTID: " ++ rawid) in
            case JD.decodeString JD.int rawid of
                Ok evid -> nocmd { mod | lasteventid = evid }
                Err err -> nocmd <| log mod ERROR <| JD.errorToString err

        GotLastEvent (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        OnRefresh ->
            ( model, refreshCmd model model.activetab )

        ActionResponse _ _ _ (Ok True) ->
            nocmd model

        ActionResponse table id action (Ok False) ->
            nocmd <| disableactions model table id

        ActionResponse table id action (Err _) ->
            nocmd <| disableactions model table id

        RelaunchMsg taskid (Ok 0) ->
            nocmd <| disableactions model TasksTab taskid

        RelaunchMsg taskid (Ok _) ->
            let
                maybetask = AL.get taskid model.tasks
                newmodel =
                    case maybetask of
                        Nothing -> model
                        Just task ->
                            let
                                newtask id =
                                    Just { task | actions = matchActionResult task.status }
                            in { model | tasks = AL.update task.id newtask model.tasks }
            in
            nocmd <| newmodel

        RelaunchMsg taskid (Err _) ->
            nocmd <| disableactions model TasksTab taskid

        Tab tab ->
            ( { model | activetab = tab }
            , refreshCmd model tab
            )

        GotServices (Ok services) ->
            nocmd { model | services = AL.fromList (groupbyid services) }

        GotServices (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotLaunchers (Ok launchers) ->
            nocmd { model | launchers = AL.fromList (groupbyid launchers) }

        GotLaunchers (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotWorkers (Ok monitor) ->
            nocmd { model
                      | monitors = AL.fromList (groupbyid monitor.monitors)
                      , workers = AL.fromList (groupbyid monitor.workers)
                  }

        GotWorkers (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        OnKill wid ->
            ( disableactions model MonitorsTab wid
            , Http.get
                { url = UB.crossOrigin
                        model.baseurl
                        [ "kill-worker", String.fromInt wid ]
                        []
                , expect = Http.expectJson (ActionResponse MonitorsTab wid Kill) JD.bool
                }
            )

        OnShutdown wid ->
            ( disableactions model MonitorsTab wid
            , Http.get
                { url = UB.crossOrigin
                        model.baseurl
                        [ "shutdown-worker", String.fromInt wid ]
                        []
                , expect = Http.expectJson (ActionResponse MonitorsTab wid Shutdown) JD.bool
                }
            )

        SetDomain domain ->
            nocmd { model | domain = LS.select domain model.domain }


        -- launcher

        OpenForm lid ->
            nocmd { model | launching = Just lid }

        CloseForm ->
            nocmd { model | launching = Nothing }

        Schedule operation ->
            ( { model | launching = Nothing }
            , schedule_task operation
            )

        -- scheduler

        GotSchedulers (Ok schedulers) ->
            nocmd { model | schedulers = AL.fromList (groupbyid schedulers) }

        GotSchedulers (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        -- logging

        HandleKeyboardEvent event ->
            if event.ctrlKey && event.key == Just "e" then
                nocmd { model | logview = not model.logview }
            else
                nocmd model

        SelectDisplayLevel level ->
            nocmd { model | logdisplaylevel = level }


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
    { url = UB.crossOrigin model.baseurl
          [ "events", String.fromInt model.lasteventid ]
          []
    , expect = Http.expectString GotEvents
    }


lasteventquery model =
    { url = UB.crossOrigin model.baseurl
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
    { url = UB.crossOrigin model.baseurl
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
                    { url = UB.crossOrigin model.baseurl
                            [ "services-table-json" ] [ ]
                    , expect = Http.expectJson GotServices (JD.list decodeService)
                    }

                MonitorsTab ->
                    { url = UB.crossOrigin model.baseurl
                            [ "workers-table-json" ] [ ]
                    , expect = Http.expectJson GotWorkers decodeWorkers
                    }

                LauncherTab ->
                    { url = UB.crossOrigin model.baseurl
                            [ "launchers-table-json" ] [ ]
                    , expect = Http.expectJson GotLaunchers (JD.list decodeLauncher)
                    }

                SchedulerTab ->
                    { url = UB.crossOrigin model.baseurl
                            [ "schedulers-table-json" ] [ ]
                    , expect = Http.expectJson GotSchedulers (JD.list decodeScheduler)
                    }

    in
    Http.get query


init : JD.Value -> ( Model, Cmd Msg )
init jsonFlags =
    let
        { baseurl, domains } =
            case JD.decodeValue decodeFlags jsonFlags of
                Ok val ->
                    let
                        dom =
                            if List.length val.domains > 1 then
                                "all" :: val.domains
                            else val.domains
                    in
                    Flags val.baseurl dom

                Err _ ->
                    Flags "" [ "default" ]

        domain =
            Maybe.unwrap
                (LS.fromList domains)
                (\x -> LS.select x (LS.fromList domains))
                (List.head domains)

        model = Model
                baseurl
                AL.empty
                AL.empty
                AL.empty
                AL.empty
                AL.empty
                Nothing
                AL.empty
                TasksTab
                domain
                0
                DEBUG
                DEBUG
                []
                False
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

                LauncherTab ->
                    1000000

                SchedulerTab ->
                    1000000

                MonitorsTab ->
                    2000
    in
    Sub.batch [ Time.every refreshTime (always OnRefresh)
              , onKeyDown (JD.map HandleKeyboardEvent decodeKeyboardEvent)
              ]


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }

port schedule_task : String -> Cmd msg

