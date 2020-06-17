module Log exposing
    ( Level(..)
    , log
    , LogEntry
    , Logger
    , viewlog
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK


type Level
    = DEBUG
    | INFO
    | ERROR


type alias LogEntry =
    { level : Level
    , count : Int
    , message : String
    , id : Int
    }


type alias Logger =
    { loglevel : Level
    , logdisplaylevel : Level
    , log : List LogEntry
    }


matchlevel : Level -> Level -> Bool
matchlevel given asked =
    case ( asked, given ) of
        ( INFO, DEBUG ) -> False
        ( ERROR, DEBUG ) -> False
        ( ERROR, INFO ) -> False
        ( _, _ ) -> True


strlevel level =
    case level of
        DEBUG -> "DEBUG"
        INFO ->  "INFO"
        ERROR -> "ERROR"


levelcolor level =
    case level of
        DEBUG -> "orange"
        INFO -> "green"
        ERROR -> "red"


-- in practice your logger will be the Model (or embedded within)
-- and it will work because of row polymorphism
-- which forbids us to use a type annotation :)
-- Logger ~ { log : List LogEntry , loglevel : Level }
log logger level msg =
    if not <| matchlevel level logger.loglevel then logger else
        case logger.log of
            [] -> { logger | log = [ LogEntry level 1 msg 0 ] }
            logentry :: tail ->
                let newid = logentry.id + 1 in
                if ( logentry.level, logentry.message ) /= ( level, msg ) then
                    { logger | log = LogEntry level 1 msg newid :: logger.log }
                else
                    { logger | log = LogEntry level (logentry.count + 1) msg newid :: tail }


-- viewlog : Logger -> (Level -> msg) -> H.Html msg
viewlog logger event =
    let
        colorize level =
            HA.attribute "style" ("color:" ++ (levelcolor level) ++ ";")

        input loglevel =
            let
                name = strlevel loglevel
            in
            H.div
                [ colorize loglevel
                , HA.style "margin-right" "1em"
                ]
                [ H.input [ HA.class "form-check-input"
                          , HA.type_ "radio"
                          , HA.name name
                          , HA.checked <| loglevel == logger.logdisplaylevel
                          , HE.onClick <| event loglevel
                          ] []
                , H.label [ HA.class "form-check-label"
                          , HA.for name
                          ]
                    [ H.text name ]
                , H.span [] [ H.text " " ]
                ]

        header =
            H.div [ HA.class "form-check form-check-inline" ]
                <| [ H.span
                         [ HA.class "font-weight-bold"
                         , HA.style "margin-right" "1em"
                         ]
                         [ H.text "Display level :" ]
                   ] ++ List.map input [DEBUG, INFO, ERROR]

        -- lines

        filterline entry =
            matchlevel entry.level logger.logdisplaylevel

        repeats val =
            if val > 1 then String.fromInt val else ""

        viewline entry =
            ( String.fromInt entry.id ,  -- vdom key
              H.div
                  []
                  [ H.span
                        [ HA.class "badge badge-dark" ]
                        [ H.text <| String.fromInt entry.id ]
                  , H.span [] [ H.text " " ]
                  , H.span
                        [ colorize entry.level ]
                        [ H.text (strlevel entry.level ++ ": " ++ entry.message ++ " ") ]
                  , H.span
                      [ HA.class "badge badge-info" ]
                      [ H.text (repeats entry.count) ]
                  ]
            )

        lines = List.filter filterline logger.log

    in
    H.div
        []
        [ header
        , HK.node "div" []  <| List.map viewline lines
        ]

