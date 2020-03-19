module Main exposing (main)

import AssocList as AL
import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    AL.Dict Int Task


initialModel : Model
initialModel =
    let
        listTask =
            [ { id = 1
              , result = Failure
              , name = "crash"
              , domain = "default"
              , queued = "2020-03-11 16:06:29+0100"
              , started = "2020-03-12 10:46:08+0100"
              , finished = "2020-03-12 10:46:18+0100"
              , user = "<unknown>"
              , worker = "#7"
              , status = Failed "toto"
              , actions = [ Abort, Wait, Delete, Relaunch ]
              }
            ]

        listTuple : List Task -> List ( Int, Task )
        listTuple listtask =
            let
                creatTuple : Task -> ( Int, Task )
                creatTuple task =
                    ( task.id, task )
            in
            List.map creatTuple listtask
    in
    AL.fromList (listTuple listTask)


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


type alias Task =
    { id : Int
    , result : TaskResult
    , name : String
    , domain : String
    , queued : String
    , started : String
    , finished : String
    , user : String
    , worker : String
    , status : Status
    , actions : List Action
    }


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
        [ H.div [ HA.id "filter", HA.style "float" "right" ]
            [ H.select
                [ HA.id "domain-filter"
                , HA.name "domain-filter"
                , HA.title "domain"
                , onchange "setdomain(this)"
                ]
                [ H.option [ HA.value "default", HA.selected True ]
                    [ H.text "default" ]
                ]
            ]
        , H.h1 []
            [ H.text "Tasks Monitoring UI"
            ]
        , H.ul [ HA.id "tabs", HA.class "nav nav-tabs", role "tablist" ]
            [ li "tasks" "Tasks"
            , li "services" "Services"
            , li "workers" "Monitors"
            ]
        , H.div [ HA.class "tab-content" ]
            [ H.div
                [ HA.id "tasks"
                , HA.class "tab-pane active"
                , role "tabpanel"
                ]
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
                    , H.tbody [] (List.map renderRow (AL.values model))
                    ]
                ]
            ]
        ]


th : String -> H.Html msg
th title =
    H.th [] [ H.text title ]


type TaskResult
    = Success
    | Failure


type Status
    = Queued
    | Running
    | Done
    | Failed String
    | Aborting
    | Aborted


type Action
    = Abort
    | Wait
    | Delete
    | Relaunch


type Msg
    = OnDelete Int
    | OnWait Int
    | OnAbort Int
    | OnRelaunch Int


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
            case status of
                Queued ->
                    tdStatus "queued" ""

                Running ->
                    tdStatus "running" ""

                Done ->
                    tdStatus "done" ""

                Failed x ->
                    tdStatus "failed" x

                Aborting ->
                    tdStatus "aborting" ""

                Aborted ->
                    tdStatus "aborted" ""

        buttonAction : String -> String -> (Int -> Msg) -> H.Html Msg
        buttonAction class title msg =
            H.button
                [ HA.class class
                , HA.type_ "button"
                , HE.onClick (msg task.id)
                ]
                [ H.text title ]

        renderAction : Action -> H.Html Msg
        renderAction action =
            case action of
                Abort ->
                    buttonAction "btn btn-warning btn-sm" "abort" OnAbort

                Wait ->
                    buttonAction "btn btn-warning btn-sm" "wait" OnWait

                Delete ->
                    buttonAction "btn btn-warning btn-sm" "delete" OnDelete

                Relaunch ->
                    buttonAction "btn btn-primary btn-sm" "relaunch" OnRelaunch
    in
    H.tr []
        [ H.th [ HA.scope "row" ]
            [ H.text (String.fromInt task.id) ]
        , renderResult task.result
        , td task.domain
        , td task.queued
        , td task.started
        , td task.finished
        , td task.user
        , td task.worker
        , renderStatus task.status
        , H.td [] (List.map renderAction task.actions)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnDelete taskId ->
            AL.remove taskId model

        OnAbort taskId ->
            creatCase taskId Aborted model

        OnRelaunch taskId ->
            creatCase taskId Queued model

        OnWait taskId ->
            creatCase taskId Running model


creatCase : Int -> Status -> Model -> Model
creatCase taskId status model =
    let
        updateTasks : Task -> Task
        updateTasks task =
            { task | status = status }

        justUpate : Maybe Task -> Maybe Task
        justUpate task =
            Maybe.map updateTasks task
    in
    AL.update taskId justUpate model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
