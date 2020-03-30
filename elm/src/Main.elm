module Main exposing (Action(..), Status(..), Task, TaskResult(..), main, taskDecoder)

import AssocList as AL
import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D


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
    , worker : Int
    , status : Status
    , deathInfo : Maybe String
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
                    , H.tbody [] (List.map renderRow (AL.values model.task))
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
    | GotTasks (Result Http.Error (List Task))


type alias JsonStatus =
    { status : String
    , abort : Bool
    , traceback : Maybe String
    }


statusDecoder : D.Decoder Status
statusDecoder =
    let
        jsonStatusDecoder : D.Decoder JsonStatus
        jsonStatusDecoder =
            D.map3 JsonStatus
                (D.field "status" D.string)
                (D.field "abort" D.bool)
                (D.field "traceback" (D.nullable D.string))

        matchStatus : JsonStatus -> D.Decoder Status
        matchStatus x =
            case ( x.status, x.abort, x.traceback ) of
                ( "queued", False, _ ) ->
                    D.succeed Queued

                ( "queued", True, _ ) ->
                    D.succeed Aborting

                ( "running", False, _ ) ->
                    D.succeed Running

                ( "running", True, _ ) ->
                    D.succeed Aborting

                ( "done", True, _ ) ->
                    D.succeed Aborted

                ( "done", False, Just traceback ) ->
                    D.succeed (Failed traceback)

                ( "done", False, Nothing ) ->
                    D.succeed Done

                ( status, _, _ ) ->
                    D.fail <| "Unknown status : " ++ status
    in
    jsonStatusDecoder |> D.andThen matchStatus


matchTaskResult : Status -> TaskResult
matchTaskResult status =
    case status of
        Failed _ ->
            Failure

        _ ->
            Success


matchActionResult : Status -> List Action
matchActionResult status =
    case status of
        Running ->
            [ Abort ]

        Aborting ->
            [ Wait ]

        Done ->
            [ Relaunch, Delete ]

        _ ->
            [ Delete ]


map12 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder j
    -> D.Decoder k
    -> D.Decoder l
    -> D.Decoder value
map12 func da db dc dd de df dg dh di dj dk dl =
    let
        map4 : (i -> j -> k -> l -> value) -> D.Decoder value
        map4 funcIJKL =
            D.map4 funcIJKL di dj dk dl
    in
    D.map8 func da db dc dd de df dg dh |> D.andThen map4


decodeTask : Status -> D.Decoder Task
decodeTask status =
    map12
        Task
        (D.field "tid" D.int)
        (D.succeed <| matchTaskResult status)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.succeed "")
        (D.field "worker" D.int)
        (D.succeed status)
        (D.field "deathinfo" (D.nullable D.string))
        (D.succeed <| matchActionResult status)


taskDecoder : D.Decoder Task
taskDecoder =
    statusDecoder |> D.andThen decodeTask


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
                    buttonAction "btn btn-danger btn-sm" "abort" OnAbort

                Wait ->
                    buttonAction "btn glyphicon glyphicon-ban-circle" "wait" OnWait

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
        , td <| "#" ++ String.fromInt task.worker
        , renderStatus task.status
        , H.td [] (List.map renderAction task.actions)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        listTuple : List Task -> List ( Int, Task )
        listTuple listtask =
            let
                creatTuple : Task -> ( Int, Task )
                creatTuple task =
                    ( task.id, task )
            in
            List.map creatTuple listtask

        setTask : TaskDict -> Model
        setTask task =
            { model | task = task }
    in
    case msg of
        OnDelete taskId ->
            ( setTask <| AL.remove taskId model.task, Cmd.none )

        OnAbort taskId ->
            ( setTask <| creatCase taskId Aborted model.task, Cmd.none )

        OnRelaunch taskId ->
            ( setTask <| creatCase taskId Queued model.task, Cmd.none )

        OnWait taskId ->
            ( setTask <| creatCase taskId Running model.task, Cmd.none )

        GotTasks (Ok tasks) ->
            ( setTask <| AL.fromList (listTuple tasks), Cmd.none )

        GotTasks (Err _) ->
            ( { model | errorMessage = Just "Could not load tasks" }, Cmd.none )


creatCase : Int -> Status -> TaskDict -> TaskDict
creatCase taskId status =
    let
        updateTasks : Task -> Task
        updateTasks task =
            { task | status = status }

        justUpate : Maybe Task -> Maybe Task
        justUpate task =
            Maybe.map updateTasks task
    in
    AL.update taskId justUpate


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://rework_ui_orig.test.pythonian.fr/tasks-table"
        , expect = Http.expectJson GotTasks (D.list taskDecoder)
        }


type alias TaskDict =
    AL.Dict Int Task


type alias Model =
    { errorMessage : Maybe String
    , task : TaskDict
    }


initialModel : Model
initialModel =
    Model
        Nothing
        AL.empty


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
