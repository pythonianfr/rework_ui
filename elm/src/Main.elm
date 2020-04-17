port module Main exposing (..)

import AssocList as AL
import Browser
import Cmd.Extra exposing (withNoCmd)
import Http
import Json.Decode as D
import ReworkUI.Decoder exposing (taskDecoder)
import ReworkUI.Type
    exposing
        ( Action(..)
        , Model
        , Msg(..)
        , Status(..)
        , Task
        , TaskDict
        , TaskResult(..)
        , User(..)
        )
import ReworkUI.View exposing (view)
import Time


port refreshTasks : (Bool -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setActionModel : Int -> Action -> Model
        setActionModel taskId action =
            modifyTask taskId (updateTaskActions action) model
    in
    case msg of
        OnDelete taskId ->
            ( setActionModel taskId (Pending Delete)
            , cmdGet
                ("http://rework_ui_orig.test.pythonian.fr/delete-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson (GotBool taskId Delete) D.bool)
            )

        OnAbort taskId ->
            ( setActionModel taskId (Pending Abort)
            , cmdGet
                ("http://rework_ui_orig.test.pythonian.fr/abort-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson (GotBool taskId Abort) D.bool)
            )

        OnRelaunch taskId ->
            ( setActionModel taskId (Pending Relaunch)
            , cmdPut
                ("http://rework_ui_orig.test.pythonian.fr/relaunch-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson (RelaunchMsg taskId) D.int)
            )

        NoOperation ->
            ( model, Cmd.none )

        GotTasks (Ok tasks) ->
            ( setTask (AL.fromList (listTuple tasks)) model, Cmd.none )

        GotTasks (Err _) ->
            ( { model | errorMessage = Just "Could not load tasks" }, Cmd.none )

        DoRefresh doRefresh ->
            ( { model | doRefresh = doRefresh }, Cmd.none )

        OnRefresh ->
            ( model, refreshTasksCmd )

        GotBool taskId action (Ok True) ->
            setActionModel taskId (Completed action) |> withNoCmd

        GotBool taskId action (Ok False) ->
            setActionModel taskId (Uncompleted action) |> withNoCmd

        GotBool taskId action (Err _) ->
            setActionModel taskId (Uncompleted action) |> withNoCmd

        RelaunchMsg taskId (Ok 0) ->
            setActionModel taskId (Uncompleted Relaunch) |> withNoCmd

        RelaunchMsg taskId (Ok _) ->
            setActionModel taskId (Completed Relaunch) |> withNoCmd

        RelaunchMsg taskId (Err _) ->
            setActionModel taskId (Uncompleted Relaunch) |> withNoCmd


listTuple : List Task -> List ( Int, Task )
listTuple listtask =
    let
        creatTuple : Task -> ( Int, Task )
        creatTuple task =
            ( task.id, task )
    in
    List.map creatTuple listtask


updateTaskActions : Action -> Task -> Task
updateTaskActions action task =
    { task | actions = [ action ] }


cmdGet : String -> Http.Expect msg -> Cmd msg
cmdGet url expect =
    Http.get
        { url = url
        , expect = expect
        }


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


setTask : TaskDict -> Model -> Model
setTask task model =
    { model | task = task }


modifyTask : Int -> (Task -> Task) -> Model -> Model
modifyTask taskId modify model =
    let
        justUpate : Maybe Task -> Maybe Task
        justUpate task =
            Maybe.map modify task
    in
    setTask (AL.update taskId justUpate model.task) model


refreshTasksCmd : Cmd Msg
refreshTasksCmd =
    Http.get
        { url = "http://rework_ui_orig.test.pythonian.fr/tasks-table"
        , expect = Http.expectJson GotTasks (D.list taskDecoder)
        }


initialModel : Model
initialModel =
    Model
        Nothing
        AL.empty
        True


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, refreshTasksCmd )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ if model.doRefresh then
                        Time.every 2000 (always OnRefresh)

                      else
                        Sub.none
                    , refreshTasks DoRefresh
                    ]
        }
