port module Main exposing (..)

import AssocList as AL
import AssocList.Extra as ALE
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Cmd.Extra exposing (withNoCmd)
import Dict exposing (Dict)
import Http
import Http.Detailed as HD
import InfiniteScroll as IS
import InfiniteScroll as IS exposing (Msg(..))
import Json.Decode as JD
import Json.Encode as JE
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
        ( decodeevents
        , decodeflags
        , decodelauncher
        , decodeplan
        , decodescheduler
        , decodeworkers
        , decodeservice
        , matchactionresult
        , decodetask
        )
import Type
    exposing
        ( Action(..)
        , defaultrule
        , Flags
        , Model
        , Msg(..)
        , Status(..)
        , TabsLayout(..)
        )
import View exposing (view)
import AssocSet as AS
import Task
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


unwraperror2 : HD.Error  String -> String
unwraperror2 resp =
    case resp of
        HD.BadUrl x -> "bad url: " ++ x
        HD.Timeout -> "the query timed out"
        HD.NetworkError -> "there was a network error"
        HD.BadStatus _ body ->
            "the server signals an issue: "  ++ body
        HD.BadBody _ a b -> "we got a bad body: " ++ a ++ b


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

                LaunchersTab ->
                    mod

                SchedulersTab ->
                    mod

                PlansTab ->
                    mod
    in
    case msg of
        -- general/ui

        Noop -> nocmd model

        HttpNoop _ ->
            nocmd model

        Tab tab ->
            ( { model | activetab = tab }
            , refreshCmd model tab
            )

        SetDomain domain ->
            nocmd { model | domain = LS.select domain model.domain }

        OnRefresh ->
            ( model, refreshCmd model model.activetab )

        ActionResponse _ _ _ (Ok True) ->
            nocmd model

        ActionResponse table id action (Ok False) ->
            nocmd <| disableactions model table id

        ActionResponse table id action (Err _) ->
            nocmd <| disableactions model table id

        -- scroll

        GotInitialViewport vp ->
            nocmd { model | height = vp.scene.height }

        Resize (_, h) ->
            nocmd { model | height = h }

        LoadMore ->
            nocmd model

        ScrollMore dir ->
            let
                remain =
                    case dir of
                        Scroll st ->
                            (st.contentHeight - (round st.scrollTop))
                        _ -> 9999

                (newmodel, nextcmd) =
                    if remain > 1200 && not model.loading
                    then (model, Cmd.none)
                    else
                        ( { model | loading = True }
                        , Http.get <|
                            (tasksquery model)
                            GotTasks
                            (List.minimum <| List.map .id (AL.values model.tasks))
                            Nothing
                        )
            in
                ( newmodel
                , nextcmd
                )

        -- tasks

        ForceLoad ->
            ( { model
                  | loading = True
                  , forceload = True
              }
            , Http.get <|
                (tasksquery model)
                GotTasks
                (Just 1)
                (List.minimum <| List.map .id (AL.values model.tasks))
            )

        GotTasks (Ok rawtasks) ->
            let mod = log model INFO ("TASKS (all):" ++ rawtasks) in
            case JD.decodeString (JD.list decodetask) rawtasks of
                Ok tasks ->
                    let
                        newtasks =
                            AL.fromList <| groupbyid tasks
                        filehintcmd =
                            [ getiofilehint model tasks "input" GotInputFileHint
                            , getiofilehint model tasks "output" GotOutputFileHint
                            ]
                    in
                    ( { model
                          | tasks = AL.union model.tasks newtasks
                          , loading = False
                          , toload = List.length tasks > 0
                      }
                    , Cmd.batch filehintcmd
                    )
                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        GotTasks (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotInputFileHint (Ok rawhints) ->
            case JD.decodeString (JD.dict JD.string) rawhints of
                Ok hints ->
                    nocmd { model | inputfilehints = hints }

                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        GotInputFileHint (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotOutputFileHint (Ok rawhints) ->
            case JD.decodeString (JD.dict JD.string) rawhints of
                Ok hints ->
                    nocmd { model | outputfilehints = hints }

                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        GotOutputFileHint (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        UpdatedTasks (Ok rawtasks) ->
            let mod = log model INFO ("TASKS (subset):" ++ rawtasks) in
            case JD.decodeString (JD.list decodetask) rawtasks of
                Ok tasks ->
                    ( { model
                          | tasks = AL.union
                            (AL.fromList (groupbyid tasks))
                            model.tasks
                      }
                    , Cmd.batch [ getiofilehint model tasks "input" GotInputFileHint
                                , getiofilehint model tasks "output" GotOutputFileHint
                                ]
                    )
                Err err -> nocmd <| log model ERROR <| JD.errorToString err

        UpdatedTasks (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        GotEvents (Ok rawevents) ->
            let mod =
                    if rawevents /= "[]"
                    then log model INFO ("EVENTS: " ++ rawevents)
                    else model
            in
            case JD.decodeString decodeevents rawevents of
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
                                    Just { task | actions = matchactionresult task.status }
                            in { model | tasks = AL.update task.id newtask model.tasks }
            in
            nocmd <| newmodel

        RelaunchMsg taskid (Err _) ->
            nocmd <| disableactions model TasksTab taskid

        -- monitors/workers

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

        -- services

        GotServices (Ok services) ->
            nocmd { model | services = AL.fromList (groupbyid services) }

        GotServices (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        -- launchers

        GotLaunchers (Ok launchers) ->
            nocmd { model | launchers = AL.fromList (groupbyid launchers) }

        GotLaunchers (Err err) ->
            nocmd <| log model ERROR <| unwraperror err


        OpenForm lid ->
            nocmd { model | launching = Just lid }

        CloseForm ->
            nocmd { model | launching = Nothing }

        Schedule operation ->
            ( { model | launching = Nothing }
            , schedule_task operation
            )

        DirectSchedule launcher ->
            ( model
            , Http.request
                { method = "PUT"
                , headers = []
                , url = UB.crossOrigin model.baseurl
                        [ "schedule2" ++ "/" ++ launcher.operation ] []
                , expect = Http.expectWhatever HttpNoop
                , body = Http.jsonBody <| JE.object
                         [ ("domain" , JE.string launcher.domain)
                         , ("host", JE.string launcher.host)
                         ]
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        -- schedulers

        GotSchedulers (Ok schedulers) ->
            nocmd { model | schedulers = AL.fromList (groupbyid schedulers) }

        GotSchedulers (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        NewScheduler ->
            case model.selectedservice of
                Just service ->
                    nocmd { model
                              | selectedservice = Nothing
                              , selectedhost = Nothing
                              , selectedrule = defaultrule }

                Nothing ->
                    let firstservice =
                            List.head (AL.values model.services)
                    in
                    case firstservice of
                        Nothing ->
                            nocmd model
                        Just service ->
                            nocmd { model | selectedservice =
                                        Just (service.name, service.domain)
                                  }

        ScheduleService service domain ->
            nocmd { model | selectedservice = Just (service, domain) }

        ScheduleHost host ->
            nocmd { model | selectedhost = Just host }

        ScheduleRule rule ->
            ( { model | selectedrule = rule }
            , Http.get
                { url = UB.crossOrigin model.baseurl
                      [ "test-cron-rule" ] [ UB.string "rule" rule ]
                , expect = HD.expectString TestedRule
                }
            )

        TestedRule (Ok err) ->
            let errmsg = Tuple.second err in
            nocmd { model | lasterror = if errmsg == "" then Nothing else Just errmsg }

        TestedRule (Err err) ->
            nocmd <| log model ERROR <| unwraperror2 err

        CancelPreSchedule ->
            nocmd { model
                      | selectedservice = Nothing
                      , selectedhost = Nothing
                      , selectedrule = defaultrule
                      , lasterror = Nothing
                  }

        PreSchedule ->
            case model.selectedservice of
                Nothing -> nocmd model  -- silly dead code
                Just selectedservice ->
                    ( model
                    , pre_schedule_task ()
                    )

        PreScheduleOk m ->
            let newmodel =
                    { model
                        | selectedservice = Nothing
                        , selectedhost = Nothing
                        , selectedrule = defaultrule
                        , lasterror = Nothing
                    }
            in
            ( newmodel
            , Http.get (getschedulers newmodel)
            )

        PreScheduleFailed err ->
            nocmd <| log model ERROR err

        LaunchNow sid ->
            ( model
            , launchnow model sid
            )

        InstantLaunchFromSchedule _ ->
            nocmd model

        DeleteSched sid ->
            ( model
            , deletescheduler model sid
            )

        DeletedSched (Ok _) ->
            ( model
            , Http.get (getschedulers model)
            )

        DeletedSched (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        -- logging

        HandleKeyboardEvent event ->
            if event.ctrlKey && event.key == Just "e" then
                nocmd { model | logview = not model.logview }
            else
                nocmd model

        SelectDisplayLevel level ->
            nocmd { model | logdisplaylevel = level }

        -- filters (tasks)

        ServiceFilter name ->
            let
                filter = model.tasksfilter
                newfilter =
                    case name of
                        "" -> { filter | service = Nothing }
                        _ -> { filter | service = Just name }
            in
            nocmd { model | tasksfilter = newfilter }

        InputsFilter name ->
            let
                filter = model.tasksfilter
                newfilter =
                    case name of
                        "" -> { filter | inputs = Nothing }
                        _ -> { filter | inputs = Just name }
            in
            nocmd { model | tasksfilter = newfilter }

        StatusFilter name ->
            let
                filter = model.tasksfilter
                newfilter =
                    case name of
                        "" -> { filter | status = Nothing }
                        _ -> { filter | status = Just name }
            in
            nocmd { model | tasksfilter = newfilter }

        GotPlans (Ok plan) ->
            nocmd { model | events = plan }

        GotPlans (Err err) ->
            nocmd <| log model ERROR <| unwraperror err

        Hours hours ->
            ( { model | hours = Maybe.withDefault 1 <| String.toInt hours }
            , Http.get <| getplans model
            )


deletescheduler model sid =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = UB.crossOrigin model.baseurl
                [ "unprepare/" ++ String.fromInt sid ] []
        , expect = Http.expectString DeletedSched
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }



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


getiofilehint model tasks direction event =
    let
        taskids =
            List.map (\tid -> String.fromInt tid) <| List.map .id tasks
        payload =
            [ ("direction" , JE.string direction)
            , ("taskid", JE.list JE.string taskids)
            ]
    in
    Http.post
    { url = UB.crossOrigin model.baseurl [ "getiofilehint" ] []
    , body = Http.jsonBody <| JE.object payload
    , expect = Http.expectString event
    }


getservices model =
    { url = UB.crossOrigin model.baseurl
          [ "services-table-json" ] [ ]
    , expect = Http.expectJson GotServices (JD.list decodeservice)
    }


getschedulers model =
    { url = UB.crossOrigin model.baseurl
          [ "schedulers-table-json" ] [ ]
    , expect = Http.expectJson GotSchedulers (JD.list decodescheduler)
    }


getlaunchers model =
    { url = UB.crossOrigin model.baseurl
          [ "launchers-table-json" ] [ ]
    , expect = Http.expectJson GotLaunchers (JD.list decodelauncher)
    }


getplans model =
    { url = UB.crossOrigin model.baseurl
          [ "plans-table-json" ] [ UB.int "hours" model.hours ]
    , expect = Http.expectJson GotPlans (JD.list decodeplan)
    }


launchnow model sid =
    Http.request
    { url = UB.crossOrigin model.baseurl
          [ "launch-scheduled/" ++ String.fromInt sid ] [ ]
    , method = "PUT"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectWhatever InstantLaunchFromSchedule
    , timeout = Nothing
    , tracker = Nothing
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
                    [ eventsquery model ]

                ServicesTab ->
                    [ getservices model ]

                MonitorsTab ->
                    [ { url = UB.crossOrigin model.baseurl
                            [ "workers-table-json" ] [ ]
                      , expect = Http.expectJson GotWorkers decodeworkers
                      }
                    ]

                LaunchersTab -> [ getlaunchers model ]

                SchedulersTab -> [ getlaunchers model, getschedulers model ]

                PlansTab -> [ getplans model ]

    in
    Cmd.batch <| List.map Http.get query


loadmore direction =
    Cmd.none


init : JD.Value -> ( Model, Cmd Msg )
init jsonFlags =
    let
        { baseurl, domains } =
            case JD.decodeValue decodeflags jsonFlags of
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


        initialvp v =
            case v of
                Err err ->
                    Noop

                Ok vp ->
                    GotInitialViewport vp

        model =
            { baseurl = baseurl
            , tasks = AL.empty
            , tasksfilter =
                  { service = Nothing
                  , inputs = Nothing
                  , status = Nothing
                  }
            , workers = AL.empty
            , monitors = AL.empty
            , services = AL.empty
            , launchers = AL.empty
            , launching = Nothing
            , activetab = TasksTab
            , domain = domain
            -- loading
            , loading = True
            , toload = True
            , forceload = False
            , scroller = IS.init loadmore
            , height = 500
            , lasteventid = 0
            -- single input/output files
            , inputfilehints = Dict.empty
            , outputfilehints = Dict.empty
            -- logging
            , loglevel = DEBUG
            , logdisplaylevel = DEBUG
            , log = []
            , logview = False
            -- scheduler
            , schedulers = AL.empty
            , selectedservice = Nothing
            , selectedhost = Nothing
            , selectedrule = defaultrule
            , lasterror = Nothing
            -- plan
            , hours = 1
            , events = []
            }
    in
    ( model
    , Cmd.batch [ Http.get <| tasksquery model GotTasks Nothing Nothing
                , Http.get <| lasteventquery model
                , Http.get <| getservices model
                , Task.attempt initialvp getViewport
                ]
    )


port schedule_task : String -> Cmd msg

port pre_schedule_task : () -> Cmd msg
port pre_schedule_fail : (String -> msg) -> Sub msg
port pre_schedule_ok : (String -> msg) -> Sub msg


sub : Model -> Sub Msg
sub model =
    let
        refreshTime =
            case model.activetab of
                TasksTab ->
                    1000

                ServicesTab ->
                    10000

                LaunchersTab ->
                    1000000

                SchedulersTab ->
                    1000000

                MonitorsTab ->
                    2000

                PlansTab ->
                    10000
    in
    Sub.batch [ Time.every refreshTime (always OnRefresh)
              , onKeyDown (JD.map HandleKeyboardEvent decodeKeyboardEvent)
              , pre_schedule_fail PreScheduleFailed
              , pre_schedule_ok PreScheduleOk
              , onResize (\w h -> Resize ( toFloat w, toFloat h ))
              ]


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        }
