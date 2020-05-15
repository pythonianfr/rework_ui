module ReworkUI.View exposing (view)

import AssocList as AL
import Bool.Extra as BE
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import List.Selection as LS
import ReworkUI.Metadata as M
import ReworkUI.Type
    exposing
        ( Action(..)
        , Monitor
        , Model
        , Msg(..)
        , Service
        , Status(..)
        , TabsLayout(..)
        , Task
        , TaskResult(..)
        , Worker
        )


role : String -> H.Attribute msg
role =
    HA.attribute "role"


aria_controls : String -> H.Attribute msg
aria_controls =
    HA.attribute "aria-controls"


data_toggle : String -> H.Attribute msg
data_toggle =
    HA.attribute "data-toggle"


aria_expanded : String -> H.Attribute msg
aria_expanded =
    HA.attribute "aria-expanded"


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
        [ H.div [ HA.id "tasks", HA.class "tab-pane active", role "tabpanel" ]
            [ H.br [] []
            , H.table
                [ HA.class """table
                               table-sm
                               table-bordered
                               table-striped
                               table-hover"""
                ]
                [ H.thead [ HA.class "thead-inverse" ]
                    [ H.tr []
                        (List.map th namesColumns)
                    ]
                , H.tbody [] htmlTable
                ]
            ]
        ]


header : LS.Selection TabsLayout -> H.Html Msg
header listTableLayout =
    H.ul [ HA.id "tabs", HA.class "nav nav-tabs", role "tablist" ]
        (LS.toList
            (LS.mapSelected
                { selected = buildLi True [ HA.class "active" ]
                , rest = buildLi False []
                }
                listTableLayout
            )
        )


strTableLayout : TabsLayout -> String
strTableLayout tableLayout =
    case tableLayout of
        TasksTab ->
            "Tasks"

        MonitorsTab ->
            "Workers"

        ServicesTab ->
            "Services"


buildLi : Bool -> List (H.Attribute Msg) -> TabsLayout -> H.Html Msg
buildLi bool listAttributes tableLayout =
    let
        newTableName =
            strTableLayout tableLayout
    in
    H.li (listAttributes ++ [ role "presentation" ])
        [ H.a
            [ HE.onClick (Tab tableLayout)
            , aria_controls (String.toLower newTableName)
            , data_toggle "tab"
            , role "tabe"
            , aria_expanded (String.toLower (BE.toString bool))
            ]
            [ H.text newTableName ]
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

        listTables =
            [ TasksTab, ServicesTab, MonitorsTab ]
                |> LS.fromList
                |> LS.select model.activetab
    in
    case model.activetab of
        TasksTab ->
            let
                head =
                    header listTables

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

                table =
                    body columnsName
                        (List.map taskRenderRow (AL.values model.tasks))

                viewerror error =
                    H.div [] [ H.text error ]

                errors =
                    List.map viewerror model.errors
            in
            if List.length model.errors > 0 then
                H.div [] errors
            else
            H.div [] [ select, title, head, table ]

        ServicesTab ->
            let
                head =
                    header listTables

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
            H.div [] [ select, title, head, table ]

        MonitorsTab ->
            let
                head =
                    header listTables

                columnsNameDomain =
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
                    body columnsNameDomain
                        (List.map
                            monitorRenderRow
                            (AL.values model.monitors)
                        )

                tableWorker =
                    body columnsNameWorker
                        (List.map workerRenderRow (AL.values model.workers))
            in
            H.div [] [ select, title, head, tableDomain, tableWorker ]


th : String -> H.Html msg
th title =
    H.th [] [ H.text title ]


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
                            [ H.a
                                [ HA.title "show the error"
                                , HA.target "_blank"
                                , HA.href ("taskerror/" ++ String.fromInt task.id)
                                ]
                                [ H.text "[traceback]" ]
                            ]
                        ]

        tdStatus : String -> String -> H.Html msg
        tdStatus class title =
            H.td [ HA.class class, HA.title title ]
                [ H.text class ]

        renderStatus : Status -> H.Html msg
        renderStatus status =
            let
                title =
                    Maybe.withDefault "" task.deathInfo
            in
            case status of
                Queued ->
                    tdStatus "queued" title

                Running ->
                    tdStatus "running" title

                Done ->
                    tdStatus "done" title

                Failed x ->
                    tdStatus "failed" x

                Aborting ->
                    tdStatus "aborting" title

                Aborted ->
                    tdStatus "aborted" title
    in
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt task.id) ]
        , renderResult task.result
        , td task.domain
        , td <| task.queued
        , td <| Maybe.withDefault "" task.started
        , td <| Maybe.withDefault "" task.finished
        , td <| user2String task.metadata
        , td <| case task.worker of
                    Nothing -> "#"
                    Just worker -> "#" ++ String.fromInt worker
        , renderStatus task.status
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
        ( disabled, prefix, newAction ) =
            case action of
                Pending act ->
                    ( True, "Pending ", act )

                Completed act ->
                    ( True, "Done ", act )

                Uncompleted act ->
                    ( True, "Failed ", act )

                _ ->
                    ( False, "", action )

        buttonAction : String -> String -> (Int -> Msg) -> H.Html Msg
        buttonAction class title msg =
            H.button
                [ HA.class class
                , HA.type_ "button"
                , HE.onClick (msg id)
                , HA.disabled disabled
                ]
                [ H.text (prefix ++ title) ]
    in
    case newAction of
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
