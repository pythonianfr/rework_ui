module Info exposing (main)

import Browser
import Decoder exposing (decodeInputspec)
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as D
import String.Extra as SE
import Type exposing (Task, InputSpec, SpecType(..))
import Url.Builder as UB


nocmd model = ( model, Cmd.none )


type alias Info =
    { state : String
    , queued : String
    , started : String
    , finished : String
    , inputspec : List InputSpec
    , outputspec : List InputSpec
    }


type alias Model =
    { baseurl : String
    , taskid : Int
    , info : Maybe Info
    }


type Msg
    = GotTaskinfo (Result Http.Error String)


infodecoder =
    D.map6 Info
        (D.field "state" D.string)
        (D.field "queued" D.string)
        (D.field "started" D.string)
        (D.field "finished" D.string)
        (D.field "inputspec" (D.list decodeInputspec))
        (D.field "outputspec" (D.list decodeInputspec))


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
        , case model.info of
              Just info -> viewinfo info
              Nothing -> H.p [] []
        ]


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

