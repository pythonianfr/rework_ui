module Info exposing (main)

import Browser
import Html as H
import Http
import Type exposing (Task)


nocmd model = ( model, Cmd.none )


type alias Model =
    { baseurl : String
    , task : Maybe Task
    }


type Msg
    = GotTaskinfo (Result Http.Error String)



view : Model -> H.Html Msg
view model =
    H.div [] []


update msg model =
    ( model, Cmd.none )


init : { baseurl : String, taskid : Int } -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model flags.baseurl Nothing
    in
        ( model, Cmd.none )


subscriptions model = Sub.none


type alias Flags =
    { baseurl : String
    , taskid : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

