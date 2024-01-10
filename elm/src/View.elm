module View exposing (view, strstatus)

import AssocList as AL
import Bool.Extra as BE
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as ARIA
import Html.Events as HE
import InfiniteScroll as IS
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


body : List String -> List (H.Html Msg) -> List (H.Html Msg) -> H.Html Msg
body columns filter htmlbody =
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
                        ([ H.tr [] (List.map th columns) ] ++ filter)
                  , H.tbody [] htmlbody
                  ]
              ]
        ]


header model tabs =
    H.ul [ HA.id "tabs"
         , HA.class "nav nav-tabs"
         , HA.attribute "role" "tablist"
         ]
        <| LS.toList
        <| LS.mapSelected
            { selected = maketab model True
            , rest = maketab model False
            }
            tabs


strtab tablelayout =
    case tablelayout of
        TasksTab ->
            "Tasks"

        MonitorsTab ->
            "Workers"

        ServicesTab ->
            "Services"

        LaunchersTab ->
            "Launchers"

        SchedulersTab ->
            "Schedulers"

        PlansTab ->
            "Plan"


strstatus task =
    case task.status of
        Queued -> "queued"
        Running -> "running"
        Done -> "done"
        Failed x -> "failed"
        Aborting -> "aborting"
        Aborted -> "aborted"


maketab model active tab =
    let
        tabname = strtab tab
        taskextra =
            if (not model.forceload) then
                [ H.span
                      [ HA.title "Force loading all the tasks."
                      , HE.onClick ForceLoad ]
                      [ H.text "⟳" ]
                ]
            else
                if model.loading then
                    [ H.div
                          [ HA.class "spinner-border spinner-border-sm text-info"
                          , ARIA.role "status"
                          ]
                          [ H.span
                                [ HA.class "sr-only" ]
                                [ H.text " ... loading" ]
                          ]
                    ]
                else [ ]
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
            [ H.div []
                  ([ H.text <| tabname ++ " " ]
                   ++ case tabname of
                          "Tasks" -> taskextra
                          _ -> []
                  )
            ]
        ]


viewdomainfilter model =
    let
        option selected x =
            H.option (selected ++ [ HA.value x ]) [ H.text x ]

    in H.div [ HA.id "filter", HA.style "float" "right" ]
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
                   model.domain |> LS.toList
              )
        ]


view : Model -> H.Html Msg
view model =
    let
        title =
            H.h1 [] [ H.text "Tasks Monitoring UI" ]

        tabs =
            [ TasksTab, MonitorsTab, LaunchersTab, SchedulersTab, PlansTab, ServicesTab ]
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
                    header model tabs

                columns =
                    [ "#"
                    , "service"
                    , "domain"
                    , "input"
                    , "events"
                    , "user"
                    , "worker"
                    , "status"
                    , "action"
                    ]


                statuses =
                    [ ""
                    , "queued"
                    , "running"
                    , "done"
                    , "failed"
                    , "aborting"
                    , "aborted"
                    ]

                makestatusfilteroption opt =
                    H.option
                        [ HA.value opt ]
                        [ H.text opt ]

                filter =
                    [ H.tr []
                          [ H.th [] []
                          , H.th []
                              [ H.input
                                    [ HA.placeholder "service filter"
                                    , HA.name "servicefilter"
                                    , HE.onInput ServiceFilter
                                    ] []
                              ]
                          , H.th [] []
                          , H.th []
                              [ H.input
                                    [ HA.placeholder "inputs filter"
                                    , HA.name "inputsfilter"
                                    , HE.onInput InputsFilter
                                    ] []
                              ]
                          , H.th [] []
                          , H.th [] []
                          , H.th [] []
                          , H.th []
                              [ H.select
                                    [ HA.name "task-status"
                                    , HA.id "task-status"
                                    , HE.on "change" <| JD.map StatusFilter HE.targetValue
                                    ]
                                    (List.map makestatusfilteroption statuses)
                              ]
                          , H.th [] []
                          ]
                    ]

                domain = Maybe.withDefault "all" <| LS.selected model.domain

                filterdomain task  =
                    case domain of
                        "all" -> True
                        _ -> domain == task.domain

                filterservice task =
                    case model.tasksfilter.service of
                        Nothing -> True
                        Just servicepart ->
                            String.contains servicepart task.name

                filterinput task =
                    let
                        taskinput =
                            case task.input of
                                Nothing -> ""
                                Just aninput -> aninput
                    in
                    case model.tasksfilter.inputs of
                        Nothing -> True
                        Just inputpart ->
                            String.contains inputpart taskinput

                filterstatus task =
                    let
                        taskstatus =
                            case task.status of
                                Queued -> "queued"
                                Running -> "running"
                                Done -> "done"
                                Failed _ -> "failed"
                                Aborting -> "aborting"
                                Aborted -> "aborted"
                    in
                    case model.tasksfilter.status of
                        Nothing -> True
                        Just fstatus -> fstatus == taskstatus

                filtertask task =
                    (filterdomain task) &&
                    (filterservice task) &&
                    (filterinput task) &&
                    (filterstatus task)

                height =
                    (String.fromInt <| ceiling model.height) ++ "px"

                scroller =
                    if model.toload && not model.loading
                    then
                        [ HA.style "height" height
                        , HA.style "overflow" "scroll"
                        , IS.infiniteScroll ScrollMore
                        ]
                    else
                        []

                table =
                    body columns filter
                        (List.map (taskrenderrow model)
                             <| List.filter filtertask (AL.values model.tasks)
                        )

            in
            H.div ([ topmargin ] ++ scroller)
                [ title, viewdomainfilter model, head, table ]

        ServicesTab ->
            let
                head =
                    header model tabs

                columns =
                    [ "#"
                    , "host"
                    , "name"
                    , "path"
                    , "domain"
                    ]

                table =
                    body columns []
                        (List.map servicerenderrow (AL.values model.services))
            in
            H.div [ topmargin ] [ title, head, table ]

        LaunchersTab ->
            let
                head = header model tabs
                columns =
                    [ "#"
                    , "operation"
                    , "domain"
                    , "host"
                    , "form"
                    ]
                table =
                    body columns []
                        (List.map
                             (launcherrenderrow model.launching)
                             (AL.values model.launchers))

            in
            H.div [ topmargin ] [ title, head, table ]

        MonitorsTab ->
            let
                head =
                    header model tabs

                monitorcolumns =
                    [ "#"
                    , "domain"
                    , "seen last"
                    , "options"
                    ]

                workercolumns =
                    [ "#"
                    , "pid@host"
                    , "domain"
                    , "memory (Mb)"
                    , "cpu"
                    , "debug port"
                    , "started"
                    , "action"
                    ]

                domaintable =
                    body monitorcolumns []
                        (List.map
                            monitorrenderrow
                            (AL.values model.monitors)
                        )

                workertable =
                    body workercolumns []
                        (List.map workerrendertow (AL.values model.workers))
            in
            H.div [ topmargin ] [ title, head, domaintable, workertable ]

        SchedulersTab ->
            let
                head =
                    header model tabs

                columns =
                    [ "#"
                    , "service"
                    , "domain"
                    , "host"
                    , "rule"
                    , "input"
                    , "action"
                    ]

                domain = Maybe.withDefault "all" <| LS.selected model.domain
                indomain sched =
                    case domain of
                        "all" -> True
                        _ -> sched.domain == domain

                table =
                    body columns []
                        (List.map schedulerrenderrow
                             (List.filter indomain (AL.values model.schedulers)))

            in
            H.div
                [ topmargin ]
                [ title, viewdomainfilter model, head, table, scheduleaction model ]

        PlansTab ->
            let
                head =
                    header model tabs

                columns =
                    [ "#"
                    , "timestamp"
                    , "operation"
                    , "domain"
                    ]

                domain = Maybe.withDefault "all" <| LS.selected model.domain
                filterdomain item =
                    case domain of
                        "all" -> True
                        dom -> item.domain == dom

                events = List.filter filterdomain model.events

                table =
                    body columns []
                        (List.map planrenderrow events)

            in
            H.div
                [ topmargin ]
                [ title, viewdomainfilter model, head, table ]


th : String -> H.Html msg
th title =
    H.th [] [ H.text title ]


validrule model =
    let rules = String.split " " model.selectedrule in
    (List.length (List.filter (\x -> String.length x > 0) rules) == 6)


findandrenderinput model service =
    -- find the relevant input using the selected service
    let
        findfirst alist matchfunc =
            case alist of
                [] -> Nothing
                (head::tail) ->
                    if matchfunc head
                    then Just head
                    else findfirst tail matchfunc

        matchservice launcher =
            (launcher.operation == Tuple.first service) &&
            (launcher.domain == Tuple.second service)

        maybelauncher = findfirst (AL.values model.launchers) matchservice
    in
    case maybelauncher of
        Nothing ->
            H.span [] []
        Just alauncher ->
            H.fieldset [ HA.class "form-group" ]
                <| List.map renderinput alauncher.inputs


scheduleaction model =
    let serviceoption service =
            H.option
                [ HA.value (service.name ++ ":" ++ service.domain) ]
                [ H.text (service.name ++ " (" ++ service.domain ++ ")") ]

        unpackserviceoption nameplusdomain =
            case String.split ":" nameplusdomain of
                [name, domain] ->
                    JD.succeed <| ScheduleService name domain
                _ ->
                    JD.fail "never there"

        serviceinput =
            H.div [ HA.class "form-group" ]
                [ H.select [ HA.name "service"
                           , HE.on "change" <|
                               (JD.andThen unpackserviceoption HE.targetValue)
                           , HA.class "form-control" ]
                      <| List.map
                      serviceoption
                      (AL.values model.services)
                ]
    in
    case model.selectedservice of
        Nothing ->
            H.button
                [ HA.class "btn btn-primary"
                , HE.onClick NewScheduler ]
                [ H.text "Schedule Task" ]
        Just service ->
            H.form [ HA.id "pre-schedule-form" ]
                [ serviceinput
                , H.div [ HA.class "form-group" ]
                    [ H.input [ HA.type_ "text"
                              , HA.name "host"
                              , HE.onInput ScheduleHost
                              , HA.placeholder "host" ] []
                    ]
                , H.div [ HA.class "form-group" ] [
                       H.input [ HA.type_ "text"
                               , HA.name "rule"
                               , HA.value model.selectedrule
                               , HE.onInput ScheduleRule
                               , HA.placeholder "rule" ] []
                      ]
                , findandrenderinput model service
                , H.button [ HA.class "btn btn-success"
                           , HA.type_ "button"
                           , HA.disabled (not (validrule model))
                           , HE.onClick PreSchedule ]
                    [ H.text "record" ]
                , H.span [] [ H.text " " ]
                , H.button [ HA.class "btn btn-danger"
                           , HA.type_ "button"
                           , HE.onClick CancelPreSchedule ]
                    [ H.text "cancel" ]
                , case model.lasterror of
                      Nothing -> H.span [] []
                      Just error ->
                          H.p
                              [ HA.class "text-danger" ]
                              [ H.text error ]
                , H.pre [ HA.class "text-monospace text-muted" ]
                    (List.intersperse (H.br [] [])
                        <| List.map H.text (String.lines crondocumentation)
                    )
                ]


crondocumentation = """
How to use the `rule` field: six specifiers must be provided.
Each one specifies a part of the rule.

┌────────────── second (0 - 59)
│ ┌───────────── minute (0 - 59)
│ │ ┌───────────── hour (0 - 23)
│ │ │ ┌───────────── day of the month (1 - 31)
│ │ │ │ ┌───────────── month (1 - 12)
│ │ │ │ │ ┌───────────── day of the week (0 - 6 or mon,tue,wed,thu,fri,sat,sun)
│ │ │ │ │ │
│ │ │ │ │ │
│ │ │ │ │ │
* * * * * *

│Expression Field Description
├────────────────────────────
│*          any   Fire on every value
│*/a        any   Fire every a values, starting from the minimum
│a-b        any   Fire on any value within the a-b range (a must be smaller than b)
│a-b/c      any   Fire every c values within the a-b range
│xth y      day   Fire on the x -th occurrence of weekday y within the month
│last x     day   Fire on the last occurrence of weekday x within the month
│last       day   Fire on the last day within the month
│x,y,z      any   Fire on any matching expression; can combine any number of any of the above expressions
"""


rendertaskactions model task =
    let actions =
            List.map (renderaction task.id) task.actions

        inputaction =
            case Dict.get (String.fromInt task.id) model.inputfilehints of
                Just filename ->
                    Just <| H.a
                        [ HA.class "btn btn-info btn-sm"
                        , HA.type_ "button"
                        , HA.title filename
                        , HA.download filename
                        , HA.href
                          <| model.baseurl
                          ++ "/getiofile/"
                          ++ (String.fromInt task.id)
                          ++ "?getfile="
                          ++ filename
                          ++ "&direction=input"
                        ]
                    [ H.text "input" ]

                Nothing -> Nothing

        outputaction =
            case Dict.get (String.fromInt task.id) model.outputfilehints of
                Just filename ->
                    Just <| H.a
                        [ HA.class "btn btn-success btn-sm"
                        , HA.type_ "button"
                        , HA.title filename
                        , HA.download filename
                        , HA.style "margin-left" ".25em"
                        , HA.href
                          <| model.baseurl
                          ++ "/getiofile/"
                          ++ (String.fromInt task.id)
                          ++ "?getfile="
                          ++ filename
                          ++ "&direction=output"
                        ]
                    [ H.text "result" ]

                Nothing -> Nothing

    in
    actions
        ++ (case inputaction of
                Nothing -> []
                Just moreaction -> [ moreaction ]
           )
        ++ (case outputaction of
                Nothing -> []
                Just moreaction -> [ moreaction ]
           )


nbsp =
    String.fromChar (Char.fromCode 160)


taskrenderrow model task =
    let
        result =
            H.span []
                [ H.a
                    [ HA.title "show the task log (if any)"
                    , HA.target "_blank"
                    , HA.href ("tasklogs/" ++ String.fromInt task.id)
                    ]
                    [ H.text task.name ]
                , H.text nbsp
                , H.a
                    [ HA.title "show task informations"
                    , HA.target "_blank"
                    , HA.href ("taskinfo/" ++ String.fromInt task.id)
                    ]
                    [ H.text "\u{1F441}" ]
                ]

        renderresult taskresult =
            case taskresult of
                Success ->
                    H.td [] [ result ]

                Failure ->
                    H.td []
                        [ result
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

        tsstatus status info =
            H.td [ HA.class status, HA.title info ]
                [ H.text status ]

        renderstatus =
            let
                info =
                    Maybe.withDefault "" task.deathInfo
                status = strstatus task
            in
            tsstatus status info

        user = user2String task.metadata
    in
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt task.id) ]
        , renderresult task.result
        , td task.domain
        , td <| Maybe.withDefault "" task.input
        , H.td [ HA.class "text-monospace", HA.style "font-size" ".8em" ]
            [ H.span [ HA.style "color" "grey" ] [ H.text task.queued ]
            , H.br [] []
            , H.span [] [ H.text <| Maybe.withDefault "" task.started ]
            , H.br [] []
            , H.span [] [ H.text <| Maybe.withDefault "" task.finished ]
            ]
        , H.td
            [ if user == unknownuser
              then HA.style "color" "grey"
              else HA.style "color" "blue"
            ]
            [ H.text user ]
        , td <| case task.worker of
                    Nothing -> "#"
                    Just worker -> "#" ++ String.fromInt worker
        , renderstatus
        , H.td [] <| rendertaskactions model task
        ]


td : String -> H.Html msg
td x =
    H.td [] [ H.text x ]


servicerenderrow : Service -> H.Html Msg
servicerenderrow service =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt service.id) ]
        , td service.host
        , td service.name
        , td service.path
        , td service.domain
        ]


schedulerrenderrow : Scheduler -> H.Html Msg
schedulerrenderrow sched =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt sched.id) ]
        , td sched.service
        , td sched.domain
        , td sched.host
        , td sched.rule
        , td (Maybe.withDefault "" sched.input)
        , H.td [] [
               H.button [ HA.class "btn btn-primary"
                        , HA.type_ "button"
                        , HE.onClick (LaunchNow sched.id)
                        ]
                   [ H.text "launch" ]
              , H.button [ HA.class "btn btn-outline-danger"
                         , HA.type_ "button"
                         , HE.onClick (DeleteSched sched.id)
                         ]
                   [ H.text "delete" ]
              ]
        ]


planrenderrow row =
    H.tr [ ]
        [ H.th [ HA.scope "row" ]
              [ H.text (String.fromInt row.id) ]
        , td row.timestamp
        , td row.operation
        , td row.domain
        ]


renderinput input =
    case input.spectype of
        Num ->
            H.div [ HA.class "form-group" ]
                [ H.input
                      [ HA.type_ "text"
                      , HA.placeholder input.name
                      , HA.required input.required
                      , HA.name input.name  ] []
                ]

        Bool ->
            let
                radios =
                    [ H.label
                          [ HA.for (input.name ++ "true")
                          , HA.style "margin" ".5em"
                          ]
                          [ H.text "true" ]
                    , H.input
                          [ HA.type_ "radio"
                          , HA.id (input.name ++ "true")
                          , HA.name input.name
                          , HA.value "true"
                          ] []
                    , H.label
                        [ HA.for (input.name ++ "false")
                        , HA.style "margin" ".5em"
                        ]
                        [ H.text "false" ]
                    ,  H.input
                        [ HA.type_ "radio"
                        , HA.id (input.name ++ "false")
                        , HA.name input.name
                        , HA.value "false"
                        ] []
                    ]
            in
            H.div [ HA.class "form-group" ]
                [ H.fieldset
                      []
                      [ H.span [ ]
                            ([ H.text (input.name ++ ": ") ] ++ radios) ]
                ]

        Str ->
            case input.choices of
                Nothing ->
                    H.div [ HA.class "form-group" ]
                        [ H.input
                              [ HA.type_ "text"
                              , HA.placeholder input.name
                              , HA.required input.required
                              , HA.name input.name  ] []
                        ]
                Just choices ->
                    let
                        makeoption choice =
                            H.option [ HA.value choice ] [ H.text choice ]
                        options =
                            case input.required of
                                True ->
                                    List.map makeoption choices
                                False ->
                                    [ H.option [] [] ] ++
                                        (List.map makeoption choices)
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

        Moment ->
            H.div [ HA.class "form-group" ]
                [ H.input
                      [ HA.type_ "text"
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


inputsrenderrow : Launcher -> H.Html Msg
inputsrenderrow launcher =
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

    in
    H.td [] [ renderTop (List.map renderinput launcher.inputs) ]


launcherrenderrow : Maybe Int -> Launcher -> H.Html Msg
launcherrenderrow launching launcher =
    let
        preform =
            case launcher.inputs of
                [] ->
                    H.td [ HE.onClick (DirectSchedule launcher) ]
                        [ H.button [ HA.class "btn btn-success"
                                   , HA.type_ "button"
                                   ] [ H.text "launch now" ]
                        ]
                _ ->
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
              Nothing -> preform
              Just lid ->
                  if launcher.id == lid then
                      inputsrenderrow launcher
                  else
                      preform
        ]


monitorrenderrow : Monitor -> H.Html Msg
monitorrenderrow monitor =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt monitor.id) ]
        , td monitor.domain
        , formatdatecolor monitor.lastSeen monitor.delta
        , H.th []
            [ H.text (String.join ", " (List.map equaltuple monitor.options)) ]
        ]


formatdatecolor : String -> Float -> H.Html Msg
formatdatecolor stringDate delta =
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


equaltuple : ( String, Int ) -> String
equaltuple tuple =
    Tuple.first tuple
        ++ "="
        ++ String.fromInt (Tuple.second tuple)


workerrendertow : Worker -> H.Html Msg
workerrendertow worker =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt worker.id) ]
        , td (String.fromInt worker.pid ++ "@" ++ worker.host)
        , td worker.domain
        , td (String.fromInt worker.mem)
        , td (String.fromFloat (worker.cpu / 100))
        , td (Maybe.map String.fromInt worker.debugPort |> Maybe.withDefault "")
        , td worker.started
        , H.td [] (List.map (renderaction worker.id) worker.actions)
        ]


renderaction : Int -> Action -> H.Html Msg
renderaction id action =
    let
        disabled =
            case action of
                Disabled act -> True
                _ -> False

        buttonaction class title msg =
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
            buttonaction
                "btn btn-danger btn-sm"
                "abort"
                OnAbort

        Delete ->
            buttonaction
                "btn btn-warning btn-sm"
                "delete"
                OnDelete

        Relaunch ->
            buttonaction
                "btn btn-primary btn-sm"
                "relaunch"
                OnRelaunch

        Kill ->
            buttonaction
                "btn btn-warning btn-sm"
                "kill"
                OnKill

        Shutdown ->
            buttonaction
                "btn btn-danger btn-sm"
                "shutdown"
                OnShutdown

        _ ->
            H.text ""
