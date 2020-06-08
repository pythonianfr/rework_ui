module Log exposing
    ( Level(..)
    , log
    , Logger
    , viewlog
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE


type Level
    = DEBUG
    | INFO
    | ERROR


type alias Logger =
    { loglevel : Level
    , logdisplaylevel : Level
    , log : List ( Level, Int, String )
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
-- Logger ~ { log : List ( Level, String ), loglevel : Level }
log logger level msg =
    if not <| matchlevel level logger.loglevel then logger else
        case logger.log of
            [] -> { logger | log = ( level, 1, msg ) :: logger.log }
            (hlevel, hcount, hmsg) :: tail ->
                if ( hlevel, hmsg ) /= ( level, msg ) then
                    { logger | log = ( level, 1, msg ) :: logger.log }
                else
                    { logger | log = ( level, hcount + 1, msg ) :: tail }


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

        filterline ( level, count, msg ) =
            matchlevel level logger.logdisplaylevel

        repeats val =
            if val > 1 then String.fromInt val else ""

        viewline ( level, count, msg ) =
            H.div
                []
                [ H.span
                      [ colorize level ]
                      [ H.text (strlevel level ++ ": " ++ msg ++ " ") ]
                , H.span
                    [ HA.class "badge badge-info" ]
                    [ H.text (repeats count) ]
                ]

        lines = List.filter filterline logger.log

    in
    H.div
        [ HA.style "margin" ".5em"]
        ([ header ] ++ List.map viewline lines)
