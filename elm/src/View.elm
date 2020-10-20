module View exposing (view, strstatus)

import AssocList as AL
import Bool.Extra as BE
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import List.Selection as LS
import Log exposing
    ( Level(..)
    , viewlog
    )
import Metadata as M
import String.Extra as SE
import Type
    exposing
        ( Action(..)
        , InputSpec
        , Launcher
        , Monitor
        , Model
        , Msg(..)
        , Scheduler
        , Service
        , SpecType(..)
        , Status(..)
        , TabsLayout(..)
        , Task
        , TaskResult(..)
        , Worker
        )


unknownuser = "<unknown>"


user2String : Maybe M.Metadata -> String
user2String metadata =
    case metadata of
        Nothing -> unknownuser
        Just meta ->
            case Dict.get "user" meta of
                Nothing ->
                    unknownuser
                Just item ->
                    M.metavaltostring item


body : List String -> List (H.Html Msg) -> H.Html Msg
body namesColumns htmlTable =
    H.div [ HA.class "tab-content" ]
        [ H.div
              [ HA.id "tasks"
              , HA.class "tab-pane active"
              , HA.attribute "role" "tabpanel"
              ]
              [ H.br [] []
              , H.table
                  [ HA.class "table table-sm table-bordered table-striped table-hover" ]
                  [ H.thead [ ]
                        [ H.tr [] (List.map th namesColumns) ]
                  , H.tbody [] htmlTable
                  ]
              ]
        ]


header tabs =
    H.ul [ HA.id "tabs"
         , HA.class "nav nav-tabs"
         , HA.attribute "role" "tablist"
         ]
        <| LS.toList
        <| LS.mapSelected
            { selected = maketab True
            , rest = maketab False
            }
            tabs


strtab tableLayout =
    case tableLayout of
        TasksTab ->
            "Tasks"

        MonitorsTab ->
            "Workers"

        ServicesTab ->
            "Services"

        LauncherTab ->
            "Launch"

        SchedulerTab ->
            "Schedulers"


strstatus task =
    case task.status of
        Queued -> "queued"
        Running -> "running"
        Done -> "done"
        Failed x -> "failed"
        Aborting -> "aborting"
        Aborted -> "aborted"


maketab active tab =
    let
        tabname = strtab tab
    in
    H.li
        [HA.class "nav-item" ]
        [ H.a
              ([ HE.onClick (Tab tab)
               , HA.class "nav-link"
               , HA.attribute "data-toggle" "tab"
               , HA.attribute "role" "tab"
               , HA.attribute "aria-selected" (if active then "true" else "false")
               , HA.id tabname
               ] ++ if active then [ HA.class "active" ] else []
              )
            [ H.text tabname ]
        ]


view : Model -> H.Html Msg
view model =
    let
        title =
            H.h1 [] [ H.text "Tasks Monitoring UI" ]

        option selected x =
            H.option (selected ++ [ HA.value x ]) [ H.text x ]

        select =
            H.div [ HA.id "filter", HA.style "float" "right" ]
                [ H.select
                    [ HA.id "domain-filter"
                    , HA.name "domain-filter"
                    , HA.title "domain"
                    , HE.on "change" (JD.map SetDomain HE.targetValue)
                    ]
                    (LS.mapSelected
                        { selected = option [ HA.selected True ]
                        , rest = option []
                        }
                        model.domain
                        |> LS.toList
                    )
                ]

        tabs =
            [ TasksTab, ServicesTab, LauncherTab, MonitorsTab, SchedulerTab ]
                |> LS.fromList
                |> LS.select model.activetab

        topmargin = HA.style "margin" ".5em"
    in
    if model.logview then
        H.div [ HA.style "margin" ".5em" ] [ viewlog model SelectDisplayLevel ]
    else
    case model.activetab of
        TasksTab ->
            let
                head =
                    header tabs

                columnsName =
                    [ "#"
                    , "service"
                    , "domain"
                    , "queued"
                    , "started"
                    , "finished"
                    , "user"
                    , "worker"
                    , "status"
                    , "action"
                    ]

                domain = Maybe.withDefault "all" <| LS.selected model.domain

                filtertask task  =
                    case domain of
                        "all" -> True
                        _ -> domain == task.domain

                table =
                    body columnsName
                        (List.map taskRenderRow
                             <| List.filter filtertask (AL.values model.tasks)
                        )

            in
            H.div [ topmargin ] [ select, title, head, table ]

        ServicesTab ->
            let
                head =
                    header tabs

                columnsName =
                    [ "#"
                    , "host"
                    , "name"
                    , "path"
                    , "domain"
                    ]

                table =
                    body columnsName
                        (List.map serviceRenderRow (AL.values model.services))
            in
            H.div [ topmargin ] [ select, title, head, table ]

        LauncherTab ->
            let
                head = header tabs
                columnsName =
                    [ "#"
                    , "operation"
                    , "domain"
                    , "host"
                    , "form"
                    ]
                table =
                    body columnsName
                        (List.map
                             (launcherRenderRow model.launching)
                             (AL.values model.launchers))

            in
            H.div [ topmargin ] [ select, title, head, table ]

        MonitorsTab ->
            let
                head =
                    header tabs

                columnsNameMonitor =
                    [ "#"
                    , "domain"
                    , "seen last"
                    , "options"
                    ]

                columnsNameWorker =
                    [ "#"
                    , "pid@host"
                    , "domain"
                    , "memory (Mb)"
                    , "cpu"
                    , "debug port"
                    , "started"
                    , "action"
                    ]

                tableDomain =
                    body columnsNameMonitor
                        (List.map
                            monitorRenderRow
                            (AL.values model.monitors)
                        )

                tableWorker =
                    body columnsNameWorker
                        (List.map workerRenderRow (AL.values model.workers))
            in
            H.div [ topmargin ] [ select, title, head, tableDomain, tableWorker ]

        SchedulerTab ->
            let
                head =
                    header tabs

                columnsName =
                    [ "#"
                    , "service"
                    , "domain"
                    , "host"
                    , "rule"
                    ]

                table =
                    body columnsName
                        (List.map schedulerRenderRow (AL.values model.schedulers))

            in
            H.div [ topmargin ] [ select, title, head, table, scheduleaction model ]


th : String -> H.Html msg
th title =
    H.th [] [ H.text title ]


scheduleaction model =
    let serviceoption service =
            H.option
                [ HA.value (service.name ++ ":" ++ service.domain) ]
                [ H.text (service.name ++ " (" ++ service.domain ++ ")") ]

        serviceinput =
            H.div [ HA.class "form-group" ]
                [ H.select [ HA.name "service"
                           , HA.class "form-control" ]
                      <| List.map
                      serviceoption
                      (AL.values model.services)
                ]
    in
    case model.createscheduler of
        False ->
            H.button
                [ HA.class "btn btn-primary"
                , HE.onClick NewScheduler ]
                [ H.text "Schedule Task" ]
        True ->
            H.form []
                [ serviceinput
                , H.input [ HA.type_ "text"
                          , HA.name "host"
                          , HA.placeholder "host" ] []
                , H.input [ HA.type_ "text"
                          , HA.name "rule"
                          , HA.placeholder "rule" ] []
                ]


taskRenderRow : Task -> H.Html Msg
taskRenderRow task =
    let
        span : H.Html msg
        span =
            H.span []
                [ H.a
                    [ HA.title "show the tasks log (if any)"
                    , HA.target "_blank"
                    , HA.href ("tasklogs/" ++ String.fromInt task.id)
                    ]
                    [ H.text task.name ]
                ]

        renderResult : TaskResult -> H.Html msg
        renderResult taskResult =
            case taskResult of
                Success ->
                    H.td []
                        [ span
                        ]

                Failure ->
                    H.td []
                        [ span
                        , H.span []
                            [ H.span [] [ H.text " " ]
                            , H.a
                                [ HA.title "show the error"
                                , HA.target "_blank"
                                , HA.style "color" "red"
                                , HA.href ("taskerror/" ++ String.fromInt task.id)
                                ]
                                [ H.text " [traceback]" ]
                            ]
                        ]

        tdStatus : String -> String -> H.Html msg
        tdStatus status info =
            H.td [ HA.class status, HA.title info ]
                [ H.text status ]

        renderStatus : H.Html msg
        renderStatus =
            let
                info =
                    Maybe.withDefault "" task.deathInfo
                status = strstatus task
            in
            tdStatus status info

        user = user2String task.metadata
    in
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt task.id) ]
        , renderResult task.result
        , td task.domain
        , H.td [ HA.style "color" "grey" ] [ H.text task.queued ]
        , td <| Maybe.withDefault "" task.started
        , td <| Maybe.withDefault "" task.finished
        , H.td
            [ if user == unknownuser
              then HA.style "color" "grey"
              else HA.style "color" "blue"
            ]
            [ H.text user ]
        , td <| case task.worker of
                    Nothing -> "#"
                    Just worker -> "#" ++ String.fromInt worker
        , renderStatus
        , H.td [] (List.map (renderAction task.id) task.actions)
        ]


td : String -> H.Html msg
td x =
    H.td [] [ H.text x ]


serviceRenderRow : Service -> H.Html Msg
serviceRenderRow service =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt service.id) ]
        , td service.host
        , td service.name
        , td service.path
        , td service.domain
        ]


schedulerRenderRow : Scheduler -> H.Html Msg
schedulerRenderRow sched =
    H.tr [] []


inputspecRenderRow : Launcher -> H.Html Msg
inputspecRenderRow launcher =
    let
        renderTop rest =
            H.form [ HA.id "run-form", HA.class "was-validated" ]
                ([ H.input [ HA.type_ "hidden"
                           , HA.name "domain"
                           , HA.value launcher.domain ] []
                 , H.input [ HA.type_ "hidden"
                           , HA.name "host"
                           , HA.value launcher.host ] []
                 ] ++ rest ++
                     [ H.button [ HA.class "btn btn-success"
                                , HA.type_ "button"
                                , HE.onClick (Schedule launcher.operation ) ]
                           [ H.text "launch" ]
                     , H.span [] [ H.text " " ]
                     , H.button [ HA.class "btn btn-danger"
                                , HA.type_ "button"
                                , HE.onClick CloseForm ]
                         [ H.text "cancel" ]
                     ]
                )

        renderInput input =
            case input.spectype of
                Num ->
                    H.div [ HA.class "form-group" ]
                        [ H.input
                              [ HA.type_ "text"
                              , HA.placeholder input.name
                              , HA.required input.required
                              , HA.name input.name  ] []
                        ]
                Str ->
                    if List.isEmpty input.choices then
                    H.div [ HA.class "form-group" ]
                        [ H.input
                              [ HA.type_ "text"
                              , HA.placeholder input.name
                              , HA.required input.required
                              , HA.name input.name  ] []
                        ]
                    else
                        let
                            makeoption choice =
                                H.option [ HA.value choice ] [ H.text choice ]
                            options =
                                if input.required then
                                    List.map makeoption input.choices
                                else
                                    [ H.option [] [] ] ++ (List.map makeoption input.choices)
                        in
                        H.div
                            [ HA.class "form-group" ]
                            [ H.label [ HA.for input.name
                                      , HA.class "control-label" ]
                                  [ H.text (SE.toTitleCase input.name) ]
                            , H.select [ HA.name input.name
                                       , HA.class "form-control" ]
                                options
                            ]
                Datetime ->
                    H.div [ HA.class "form-group" ]
                        [ H.input
                              [ HA.type_ "datetime-local"
                              , HA.placeholder input.name
                              , HA.required input.required
                              , HA.name input.name  ] []
                        ]

                File ->
                    H.div [ HA.class "form-group" ] [
                         H.label [ HA.for input.name
                                 , HA.class "control-label" ]
                             [ H.text (SE.toTitleCase input.name) ]
                        , H.input [ HA.type_ "file"
                                  , HA.name input.name
                                  , HA.required input.required
                                  , HA.class "form-control"
                                  ] []
                        ]
    in
    H.td [] [ renderTop (List.map renderInput launcher.inputs) ]


launcherRenderRow : Maybe Int -> Launcher -> H.Html Msg
launcherRenderRow launching launcher =
    let
        preform id =
            H.td [ HE.onClick (OpenForm launcher.id) ]
                [ H.button [ HA.class "btn btn-primary"
                           , HA.type_ "button"
                           ] [ H.text "open form" ]
                ]
    in
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt launcher.id) ]
        , td launcher.operation
        , td launcher.domain
        , td launcher.host
        , case launching of
              Nothing -> preform launcher.id
              Just lid ->
                  if launcher.id == lid then
                      inputspecRenderRow launcher
                  else
                      preform launcher.id
        ]


monitorRenderRow : Monitor -> H.Html Msg
monitorRenderRow monitor =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt monitor.id) ]
        , td monitor.domain
        , formatdateColor monitor.lastSeen monitor.delta
        , H.th []
            [ H.text (String.join ", " (List.map equalTuple monitor.options)) ]
        ]


formatdateColor : String -> Float -> H.Html Msg
formatdateColor stringDate delta =
    let
        color =
            if delta > 60 then
                "DarkRed"

            else if delta > 10 then
                "DarkMagenta"

            else
                "DarkGreen"
    in
    H.td [ HA.style "color" color ] [ H.text stringDate ]


equalTuple : ( String, Int ) -> String
equalTuple tuple =
    Tuple.first tuple
        ++ "="
        ++ String.fromInt (Tuple.second tuple)


workerRenderRow : Worker -> H.Html Msg
workerRenderRow worker =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt worker.id) ]
        , td (String.fromInt worker.pid ++ "@" ++ worker.host)
        , td worker.domain
        , td (String.fromInt worker.mem)
        , td (String.fromFloat (worker.cpu / 100))
        , td (Maybe.map String.fromInt worker.debugPort |> Maybe.withDefault "")
        , td worker.started
        , H.td [] (List.map (renderAction worker.id) worker.actions)
        ]


renderAction : Int -> Action -> H.Html Msg
renderAction id action =
    let
        disabled =
            case action of
                Disabled act -> True
                _ -> False

        buttonAction class title msg =
            if not disabled then
                H.span
                    []
                    [ H.button
                          [ HA.class class
                          , HA.type_ "button"
                          , HE.onClick (msg id)
                          ]
                          [ H.text (title) ]
                    , H.text " "
                    ]
            else H.div [] []
    in
    case action of
        Abort ->
            buttonAction
                "btn btn-danger btn-sm"
                "abort"
                OnAbort

        Delete ->
            buttonAction
                "btn btn-warning btn-sm"
                "delete"
                OnDelete

        Relaunch ->
            buttonAction
                "btn btn-primary btn-sm"
                "relaunch"
                OnRelaunch

        Kill ->
            buttonAction
                "btn btn-warning btn-sm"
                "kill"
                OnKill

        Shutdown ->
            buttonAction
                "btn btn-danger btn-sm"
                "shutdown"
                OnShutdown

        _ ->
            H.text ""
