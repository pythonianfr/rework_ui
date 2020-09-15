module Metadata
    exposing (MetaVal(..)
             , decodemeta
             , Metadata
             , metavaldecoder
             , metavaltostring
             )

import Dict exposing (Dict)
import Http
import Json.Decode as D


type MetaVal
    = MString String
    | MInt Int
    | MFloat Float
    | MBool Bool
    | MList (List MetaVal)


metavaltostring mv =
    case mv of
        MString s -> s
        MInt i -> String.fromInt i
        MFloat f -> String.fromFloat f
        MBool b -> if b then "true" else "false"
        MList l -> String.join ", " <| List.map metavaltostring l


type alias Metadata =
    Dict String MetaVal


metavaldecoder : D.Decoder MetaVal
metavaldecoder =
    D.oneOf
        [ D.map MString D.string
        , D.map MInt D.int
        , D.map MFloat D.float
        , D.map MBool D.bool
        , D.map MList (D.list (D.lazy (\_  -> metavaldecoder)))
        ]


decodemeta : D.Decoder Metadata
decodemeta =
    D.dict metavaldecoder
