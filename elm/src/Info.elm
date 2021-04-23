module Info exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as D
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


infodecoder =
    D.map4 Info
        (D.field "state" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)


getinfo model =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "info-for", String.fromInt model.taskid ] []
        , expect = Http.expectString GotTaskinfo
        }



view : Model -> H.Html Msg
view model =
    let
        taskstatus =
            case model.info of
                Just info -> info.state
                Nothing -> "N/A"
    in
    H.div [ HA.style "margin" ".5em" ]
        [ H.h2 []
          [ H.span [] [ H.text ("Task #" ++ (String.fromInt model.taskid) ++ " ") ]
          , H.small [ HA.class "badge badge-info" ] [ H.text taskstatus ]
          ]
        ]

update msg model =
    case msg of
        GotTaskinfo (Ok rawinfo) ->
            case D.decodeString infodecoder rawinfo of
                Ok info ->
                    ( { model | info = Just info }
                    , Cmd.none
                    )
                Err err ->
                    let _ = Debug.log "error" err
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

