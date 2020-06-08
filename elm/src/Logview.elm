module Logview exposing (..)

import Browser
import Http
import Json.Decode as D
import Time
import Url.Builder as UB


import Log exposing
    ( Level(..)
    , log
    , viewlog
    , Logger
    )


type alias Model =
    { baseurl : String
    , taskid : Int
    , lastlogid : Int
    , logger : Logger
    }


type alias Flags =
    { baseurl : String
    , taskid : Int
    }


type Msg
    = GotLogs (Result Http.Error String)
    | Refreshed
    | SelectDisplayLevel Level


logsquery model =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "job_logslice", String.fromInt model.taskid ]
              [ UB.int "from_log_id" model.lastlogid ]
        , expect = Http.expectString GotLogs
        }


flagsdecoder =
    D.map2 Flags
        (D.field "baseurl" D.string)
        (D.field "taskid" D.int)


nocmd model = ( model, Cmd.none )


update msg model =
    case msg of
        GotLogs (Ok rawlogs) -> nocmd model
        GotLogs (Err error) -> nocmd model
        Refreshed -> nocmd model
        SelectDisplayLevel level -> nocmd model


view model =
    viewlog model.logger SelectDisplayLevel


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        { baseurl, taskid } =
            case D.decodeValue flagsdecoder flags of
                Ok val ->
                    Flags val.baseurl val.taskid

                Err _ ->
                    Flags "" 0

        model =
            Model
                baseurl
                taskid
                0
                (Logger DEBUG DEBUG [])
    in
    ( model
    , logsquery model
    )


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Time.every 1000 (always Refreshed)
        }
