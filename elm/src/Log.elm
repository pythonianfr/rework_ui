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
    , log : List ( Level, String )
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
        DEBUG -> "D"
        INFO -> "I"
        ERROR -> "E"


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
    if matchlevel level logger.loglevel
    then { logger | log = ( level, msg ) :: logger.log }
    else logger


viewlog logger level =
    let
        filterline ( lev, msg ) =
            matchlevel lev level

        viewline ( lev, msg ) =
            H.div
                [ HA.attribute "style" ("color:" ++ (levelcolor lev) ++ ";") ]
                [ H.span [] [ H.text (strlevel lev ++ ":" ++ msg) ] ]

        lines = List.filter filterline logger.log

    in
    H.div
        [ ] <| List.map viewline lines
