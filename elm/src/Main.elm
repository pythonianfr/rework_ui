port module Main exposing (..)

import AssocList as AL
import Browser
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
    case msg of
        OnDelete taskId ->
            ( modifyTask taskId (updateTaskActions Delete) model
            , cmdGet
                ("http://rework_ui_orig.test.pythonian.fr/delete-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson GotBool D.bool)
            )

        OnAbort taskId ->
            ( modifyTask taskId (updateTaskActions Abort) model
            , cmdGet
                ("http://rework_ui_orig.test.pythonian.fr/abort-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson GotBool D.bool)
            )

        OnRelaunch taskId ->
            ( modifyTask taskId (updateTaskActions Relaunch) model
            , cmdPut
                ("http://rework_ui_orig.test.pythonian.fr/relaunch-task/"
                    ++ String.fromInt taskId
                )
                (Http.expectJson RelaunchMsg D.int)
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

        _ ->
            ( model, Cmd.none )


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
    { task | actions = [ Pending action ] }


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
