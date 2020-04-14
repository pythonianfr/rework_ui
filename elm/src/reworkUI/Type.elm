module Type exposing (..)

import AssocList as AL
import Http


type alias Task =
    { id : Int
    , result : TaskResult
    , name : String
    , domain : String
    , queued : String
    , started : String
    , finished : String
    , user : User
    , worker : Int
    , status : Status
    , deathInfo : Maybe String
    , actions : List Action
    }


type alias JsonStatus =
    { status : String
    , abort : Bool
    , traceback : Maybe String
    }


type alias JsonUser =
    { user : Maybe String
    , runName : Maybe String
    }


type alias TaskDict =
    AL.Dict Int Task


type alias Model =
    { errorMessage : Maybe String
    , task : TaskDict
    }


type User
    = UnknownUser
    | NamedUser String
    | RunUser String String


type TaskResult
    = Success
    | Failure


type Status
    = Queued
    | Running
    | Done
    | Failed String
    | Aborting
    | Aborted


type Action
    = Abort
    | Wait
    | Delete
    | Relaunch
    | Pending Action


type Msg
    = OnDelete Int
    | NoOperation
    | OnAbort Int
    | OnRelaunch Int
    | GotTasks (Result Http.Error (List Task))
    | GotBool (Result Http.Error Bool)
    | RelaunchMsg (Result Http.Error Int)
