module Logview exposing (main)

import Browser
import Http
import Json.Decode as D
import List.Extra as LE
import Maybe.Extra as Maybe
import Regex as RE
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


nocmd model = ( model, Cmd.none )

logsdecoder =
    D.list <| D.map2 Tuple.pair
        (D.index 0 D.int)
        (D.index 1 D.string)


rawlogstologentries : List (Int, String) -> List (Int, Level, String)
rawlogstologentries rawlogs =
    let
        re = Maybe.withDefault RE.never <| RE.fromString "(\\w+):(\\w+):(.*)"
        transform (lineid, line) =
            let
                matches = RE.find re line
                items = Maybe.values
                        (Maybe.withDefault []
                             <| List.head
                             <| List.map .submatches
                             <| RE.find re line
                        )
            in
            case items of
                (source::strlevel::rest) ->
                    let
                        logline = Maybe.withDefault "nope" <| List.head rest
                        level =
                            case strlevel of
                                "DEBUG" -> DEBUG
                                "INFO" -> INFO
                                "ERROR" -> ERROR
                                _ -> DEBUG
                    in (lineid, level, logline)

                _ -> ( lineid, ERROR, "could not parse the log line" )

    in
    List.map transform rawlogs


update msg model =
    case msg of
        GotLogs (Ok rawlogs) ->
            case D.decodeString logsdecoder rawlogs of
                Ok parsedlogs ->
                    let
                        -- inject into the logger the new log entries
                        -- while tracking the last log id
                        logmany : List (Int, Level, String) -> Logger -> Int -> (Logger, Int)
                        logmany parsedloglist logger curlineid =
                            case parsedloglist of
                                [] -> ( logger, curlineid )
                                (lineid, level, line) :: rest ->
                                    logmany rest (log logger level line) lineid

                        ( newlogger, lastid) =
                            logmany (rawlogstologentries parsedlogs) model.logger -1
                    in
                    nocmd { model
                              | logger = newlogger
                              , lastlogid = if lastid == -1 then model.lastlogid else lastid
                          }
                Err err -> nocmd model

        GotLogs (Err error) -> nocmd model

        Refreshed ->
            ( model
            , logsquery model
            )

        SelectDisplayLevel level -> nocmd model


view model =
    viewlog model.logger SelectDisplayLevel


init : { baseurl : String, taskid : Int } -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                flags.baseurl
                flags.taskid
                0
                (Logger DEBUG DEBUG [])
    in
    ( model
    , logsquery model
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Time.every 1000 (always Refreshed)
        }
