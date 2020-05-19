module Log exposing
    ( Level(..)
    , log
    , viewlog
    )

import Html as H
import Html.Attributes as HA


type Level
    = DEBUG
    | INFO
    | ERROR


type alias Logger =
    { loglevel : Level
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


viewlog logger level =
    let
        filterline ( lev, count, msg ) =
            matchlevel lev level

        repeats val =
            if val > 1 then String.fromInt val else ""

        viewline ( lev, count, msg ) =
            H.div
                []
                [ H.span
                      [ HA.attribute "style" ("color:" ++ (levelcolor lev) ++ ";")]
                      [ H.text (strlevel lev ++ ": " ++ msg ++ " ") ]
                , H.span
                    [ HA.class "badge badge-info" ]
                    [ H.text (repeats count) ]
                ]

        lines = List.filter filterline logger.log

    in
    H.div
        [ ] <| List.map viewline lines
