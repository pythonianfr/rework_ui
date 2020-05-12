module ReworkUI.View exposing (view)

import AssocList as AL
import Bool.Extra as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import List.Selection as LS
import ReworkUI.Type
    exposing
        ( Action(..)
        , Domain
        , Model
        , Msg(..)
        , Service
        , Status(..)
        , Table(..)
        , Task
        , TaskResult(..)
        , User(..)
        , Worker
        )


role : String -> H.Attribute msg
role =
    HA.attribute "role"


onchange : String -> H.Attribute msg
onchange =
    HA.attribute "onchange"


aria_controls : String -> H.Attribute msg
aria_controls =
    HA.attribute "aria-controls"


data_toggle : String -> H.Attribute msg
data_toggle =
    HA.attribute "data-toggle"


aria_expanded : String -> H.Attribute msg
aria_expanded =
    HA.attribute "aria-expanded"


user2String : User -> String
user2String user =
    case user of
        UnknownUser ->
            "Unknown"

        NamedUser name ->
            name

        RunUser name runName ->
            name ++ " [" ++ runName ++ "]"


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


header : List ( Bool, String ) -> H.Html Msg
header listTuple =
    H.ul [ HA.id "tabs", HA.class "nav nav-tabs", role "tablist" ]
        (List.map buildLi listTuple)


buildLi : ( Bool, String ) -> H.Html Msg
buildLi tuple =
    let
        bool =
            Tuple.first tuple

        newTableName =
            if Tuple.second tuple == "Monitors" then
                "workers"

            else
                Tuple.second tuple

        class =
            if bool then
                "active"

            else
                ""
    in
    H.li [ HA.class class, role "presentation" ]
        [ H.a
            [ HE.onClick (Table newTableName)
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
                        model.userDomain
                        |> LS.toList
                    )
                ]
    in
    case model.tableLayout of
        TableTasks ->
            let
                head =
                    header
                        [ ( True, "Tasks" )
                        , ( False, "Services" )
                        , ( False, "Monitors" )
                        ]

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
                        (List.map taskRenderRow (AL.values model.task))
            in
            H.div [] [ select, title, head, table ]

        TableServices ->
            let
                head =
                    header
                        [ ( False, "Tasks" )
                        , ( True, "Services" )
                        , ( False, "Monitors" )
                        ]

                columnsName =
                    [ "#"
                    , "host"
                    , "name"
                    , "path"
                    , "domain"
                    ]

                table =
                    body columnsName
                        (List.map serviceRenderRow (AL.values model.service))
            in
            H.div [] [ select, title, head, table ]

        TableMonitors ->
            let
                head =
                    header
                        [ ( False, "Tasks" )
                        , ( False, "Services" )
                        , ( True, "Monitors" )
                        ]

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
                            domainRenderRow
                            (AL.values model.domain)
                        )

                tableWorker =
                    body columnsNameWorker
                        (List.map workerRenderRow (AL.values model.worker))
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
        , td task.queued
        , td task.started
        , td task.finished
        , td <| user2String task.user
        , td <| "#" ++ String.fromInt task.worker
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
            [ H.text (String.fromInt service.opid) ]
        , td service.host
        , td service.name
        , td service.path
        , td service.domain
        ]


domainRenderRow : Domain -> H.Html Msg
domainRenderRow domain =
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt domain.id) ]
        , td domain.domain
        , formatdateColor domain.lastSeen domain.delta
        , H.th []
            [ H.text (String.join ", " (List.map equalTuple domain.options)) ]
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
            [ H.text (String.fromInt worker.wId) ]
        , td (String.fromInt worker.pid ++ "@" ++ worker.host)
        , td worker.domain
        , td (String.fromInt worker.mem)
        , td (String.fromFloat (worker.cpu / 100))
        , td (Maybe.map String.fromInt worker.debugPort |> Maybe.withDefault "")
        , td worker.started
        , H.td [] (List.map (renderAction worker.wId) worker.actions)
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

        Wait ->
            buttonAction
                "btn glyphicon glyphicon-ban-circle"
                "wait"
                (always NoOperation)

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
