module Info exposing (main)

import Browser
import Decoder exposing (decodeInputspec)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as D
import Metadata exposing (decodemeta, Metadata, MetaVal(..))
import String.Extra as SE
import Type exposing (Task, IOSpec, SpecType(..))
import Url.Builder as UB


nocmd model = ( model, Cmd.none )


type alias Info =
    { state : String
    , queued : String
    , started : String
    , finished : String
    , inputspec : List IOSpec
    , outputspec : List IOSpec
    }


type alias Model =
    { baseurl : String
    , taskid : Int
    , info : Maybe Info
    , inputs : Maybe Metadata
    , outputs : Maybe Metadata
    , inputfilelengths : Dict String Int
    , outputfilelengths : Dict String Int
    }


type Msg
    = GotTaskinfo (Result Http.Error String)
    | GotInputs (Result Http.Error String)
    | GotOutputs (Result Http.Error String)
    | GotInputFileLengths (Result Http.Error String)
    | GotOutputFileLengths (Result Http.Error String)


infodecoder =
    D.map6 Info
        (D.field "state" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.field "inputspec" (D.list decodeInputspec))
        (D.field "outputspec" (D.list decodeInputspec))


filelengthsdecoder =
    D.dict D.int


getinfo model =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "info-for", String.fromInt model.taskid ] []
        , expect = Http.expectString GotTaskinfo
        }


getio model direction event =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "read_io", String.fromInt model.taskid ]
              [ UB.string "direction" direction ]
        , expect = Http.expectString event
        }


getfilelengths model direction event =
    Http.get
        { url = UB.crossOrigin model.baseurl
              [ "getiofile_lengths", String.fromInt model.taskid ]
              [ UB.string "direction" direction ]
        , expect = Http.expectString event
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

        , case model.info of
              Just info -> viewinfo info
              Nothing -> H.p [] []

        , case model.inputs of
              Just inputs ->
                  H.div []
                      [ H.text "inputs → "
                      , H.div [ HA.style "margin-left" "5em" ]
                          ([ viewio inputs ] ++ viewfiles model "input")
                      ]

              Nothing -> H.div [] [ H.text "no input" ]

        , case model.outputs of
              Just outputs ->
                  H.div []
                      [ H.text "outputs → "
                      , H.div [ HA.style "margin-left" "5em" ]
                          ([ viewio outputs ] ++ viewfiles model "output")
                      ]

              Nothing -> H.div [] [ H.text "no output" ]
        ]


viewfiles model direction =
    let data =
            if direction == "input" then
                model.inputfilelengths
            else
                model.outputfilelengths

        downloadlink (filename, filesize) =
            if filesize > 0 then
                H.a [ HA.download filename
                    , HA.href
                          <| model.baseurl
                          ++ "/getiofile/"
                          ++ (String.fromInt model.taskid)
                          ++ "?getfile="
                          ++ filename
                          ++ "&direction="
                          ++ direction
                    ]
                    [ H.text filename ]
            else
                H.span [] []
    in
    List.map downloadlink
        <| List.filter (\(k, v) -> v > 0)
        <| Dict.toList data


viewio meta =
    let
        formatmeta m =
            case m of
                MString s -> s
                MInt i -> String.fromInt i
                MFloat f -> String.fromFloat f
                MBool b -> if b then "true" else "false"
                MList l -> "nope"
        viewmeta (k, m) =
            H.span [] [ H.text <| k ++ " → " ++ (formatmeta m)
                      , H.br [][]
                      ]
    in
    H.span [] (List.map viewmeta (Dict.toList meta))


show label thing =
    if String.length thing > 0 then label ++ " → " ++ thing else ""


viewrequired input =
    if input.required then " [required]" else ""


viewiospec input =
    H.div [] [
         case input.spectype of
             Num ->
                 H.text (input.name ++ " [number]" ++ (viewrequired input))

             Str ->
                 H.text <| String.join ""
                     [
                      input.name
                     , " [string] "
                     , case input.choices of
                           Nothing -> ""
                           Just choices -> " choices: " ++ String.join ", " choices
                     , viewrequired input
                     ]

             Datetime ->
                 H.text (input.name ++ " [datetime]" ++ (viewrequired input))

             Moment ->
                 H.text (input.name ++ " [moment]" ++ (viewrequired input))

             File ->
                 H.text (input.name ++ " [file]" ++ (viewrequired input))
        ]


viewinfo info =
    H.div []
        [ H.text (show "queued" info.queued)
        , H.text (show "started" info.started)
        , H.text (show "finished" info.finished)
        , H.div []
            [ H.text "input spec → "
            , H.div [ HA.style "margin-left" "5em" ]
                (List.map viewiospec info.inputspec)
            ]
         , H.div []
             [ H.text "output spec → "
             , H.div [ HA.style "margin-left" "5em" ]
                 (List.map viewiospec info.outputspec)
             ]
        ]


update msg model =
    case msg of
        GotTaskinfo (Ok rawinfo) ->
            case D.decodeString infodecoder rawinfo of
                Ok info ->
                    ( { model | info = Just info }
                    , Cmd.batch [ getio model "input" GotInputs
                                , getio model "output" GotOutputs
                                , getfilelengths model "input" GotInputFileLengths
                                , getfilelengths model "output" GotOutputFileLengths
                                ]
                    )
                Err err ->
                    ( model, Cmd.none )

        GotTaskinfo (Err e) ->
            ( model, Cmd.none )

        GotInputs (Ok rawinputs) ->
            case D.decodeString decodemeta rawinputs of
                Ok parsedio ->
                    ( { model | inputs = Just parsedio }
                    , Cmd.none
                    )
                Err error ->
                    nocmd model

        GotInputs (Err e) ->
            nocmd model

        GotOutputs (Ok rawoutputs) ->
            case D.decodeString decodemeta rawoutputs of
                Ok parsedio ->
                    ( { model | outputs = Just parsedio }
                    , Cmd.none
                    )
                Err error ->
                    nocmd model

        GotOutputs (Err e) ->
            nocmd model

        GotInputFileLengths (Ok raw) ->
            case D.decodeString filelengthsdecoder raw of
                Ok flengths ->
                    nocmd { model | inputfilelengths = flengths }
                Err error ->
                    nocmd model

        GotInputFileLengths (Err e) ->
            nocmd model

        GotOutputFileLengths (Ok raw) ->
            case D.decodeString filelengthsdecoder raw of
                Ok flengths ->
                    nocmd { model | outputfilelengths = flengths }
                Err error ->
                    nocmd model

        GotOutputFileLengths (Err e) ->
            nocmd model


init : { baseurl : String, taskid : Int } -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                flags.baseurl
                flags.taskid
                Nothing
                Nothing
                Nothing
                Dict.empty
                Dict.empty
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

