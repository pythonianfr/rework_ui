module ReworkUI.View exposing (view)

import AssocList as AL
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import ReworkUI.Type
    exposing
        ( Action(..)
        , Model
        , Msg(..)
        , Status(..)
        , Task
        , TaskResult(..)
        , User(..)
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


li : String -> String -> H.Html msg
li controle title =
    H.li [ role "presentation" ]
        [ H.a
            [ aria_controls controle
            , data_toggle "tab"
            , role "tab"
            , aria_expanded "false"
            ]
            [ H.text title ]
        ]


user2String : User -> String
user2String user =
    case user of
        UnknownUser ->
            "Unknown"

        NamedUser name ->
            name

        RunUser name runName ->
            name ++ " [" ++ runName ++ "]"


view : Model -> H.Html Msg
view model =
    let
        headers =
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
    in
    H.div []
        [ H.br [] []
        , H.table [ HA.class """table
                                table-sm
                                table-bordered
                                table-striped
                                table-hover""" ]
            [ H.thead [ HA.class "thead-inverse" ]
                [ H.tr []
                    (List.map th headers)
                ]
            , H.tbody [] (List.map renderRow (AL.values model.task))
            ]
        ]


th : String -> H.Html msg
th title =
    H.th [] [ H.text title ]


renderRow : Task -> H.Html Msg
renderRow task =
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

        td : String -> H.Html msg
        td x =
            H.td [] [ H.text x ]

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
        , H.td [] (List.map (renderAction task) task.actions)
        ]


renderAction : Task -> Action -> H.Html Msg
renderAction task action =
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
                , HE.onClick (msg task.id)
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

        _ ->
            H.text ""
