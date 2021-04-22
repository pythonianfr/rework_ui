module Info exposing (main)

import Browser
import Html as H
import Http
import Type exposing (Task)
import Url.Builder as UB


nocmd model = ( model, Cmd.none )


type alias Info =
    { state : String
    , queued : String
    , started : String
    , finished : String
    }


type alias Model =
    { baseurl : String
    , taskid : Int
    , info : Maybe Info
    }


type Msg
    = GotTaskinfo (Result Http.Error String)


getinfo model =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "info-for", String.fromInt model.taskid ] []
        , expect = Http.expectString GotTaskinfo
        }



view : Model -> H.Html Msg
view model =
    H.div [] []


update msg model =
    case msg of
        GotTaskinfo (Ok rawinfo) ->
            let _ = Debug.log "taskinfo" rawinfo
            in ( model, Cmd.none )

        GotTaskinfo (Err e) ->
            let _ = Debug.log "err" e
            in ( model, Cmd.none )


init : { baseurl : String, taskid : Int } -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model flags.baseurl flags.taskid Nothing
    in
    ( model, getinfo model )


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

