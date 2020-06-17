module Logview exposing (main)

import Browser
import Http
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import List.Extra as LE
import Main exposing (tasksquery)
import Maybe.Extra as Maybe
import Regex as RE
import ReworkUI.Decoder exposing (taskDecoder)
import ReworkUI.Type exposing (Task, Status(..))
import ReworkUI.View exposing (strstatus)
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
    , task : Maybe Task
    , lastlogid : Int
    , logger : Logger
    }


type alias Flags =
    { baseurl : String
    , taskid : Int
    }


type Msg
    = GotTask (Result Http.Error String)
    | GotLogs (Result Http.Error String)
    | Refreshed
    | SelectDisplayLevel Level


logsquery model =
    case model.task of
        Just task ->
            Http.get
                { url = UB.crossOrigin model.baseurl
                      [ "job_logslice", String.fromInt task.id ]
                      [ UB.int "from_log_id" model.lastlogid ]
                , expect = Http.expectString GotLogs
                }

        Nothing -> Cmd.none


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
        GotTask (Ok rawtask) ->
            case D.decodeString (D.list taskDecoder) rawtask of
                Ok decoded ->
                    let
                        newmodel = { model | task = List.head decoded }
                    in
                    ( newmodel
                    , logsquery newmodel
                    )

                Err err -> nocmd model

        GotTask (Err err) ->
            nocmd model

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

        SelectDisplayLevel level ->
            let
                logger = model.logger
                newlogger = { logger | logdisplaylevel = level }
            in
            nocmd { model | logger = newlogger }


view : Model -> H.Html Msg
view model =
    let
        taskid =
            case model.task of
                Just task -> String.fromInt task.id
                Nothing -> "<unknown>"
        taskstatus =
            case model.task of
                Just task -> strstatus task
                Nothing -> "N/A"
    in
    H.div [ HA.style "margin" ".5em" ]
        [ H.h2 [ ]
              [ H.span [] [ H.text ("Task #" ++ taskid ++ " ") ]
              , H.small [ HA.class "badge badge-info" ] [ H.text taskstatus ]
              ]
        , viewlog model.logger SelectDisplayLevel
        ]


init : { baseurl : String, taskid : Int } -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                flags.baseurl
                Nothing
                0
                (Logger DEBUG DEBUG [])
    in
    ( model
    , Http.get <| tasksquery model GotTask (Just flags.taskid) (Just flags.taskid)
    )


refresh model =
    let
        doit =
            case model.task of
                Nothing -> False
                Just task ->
                    case task.status of
                        Queued -> True
                        Running -> True
                        Done -> False
                        Failed x -> False
                        Aborting -> True
                        Aborted -> False

    in
    if doit then Time.every 1000 (always Refreshed) else Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = refresh
        }
