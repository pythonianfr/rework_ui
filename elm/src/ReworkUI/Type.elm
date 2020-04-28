module ReworkUI.Type exposing (..)

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


type alias Service =
    { opid : Int
    , host : String
    , name : String
    , path : String
    , domain : String
    }


type alias Domain =
    { id : Int
    , domain : String
    , delta : Float
    , lastSeen : String
    , options : List ( String, Int )
    }


type alias Worker =
    { wId : Int
    , host : String
    , pid : Int
    , domain : String
    , mem : Int
    , cpu : Float
    , debugPort : Maybe Int
    , started : String
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


type alias JsonMonitors =
    { domains : List Domain
    , workers : List Worker
    }


type alias IntDict a =
    AL.Dict Int a


type alias WorkerDict =
    IntDict Worker


type alias DomainDict =
    IntDict Domain


type alias TaskDict =
    IntDict Task


type alias ServiceDict =
    IntDict Service


type alias Model =
    { errorMessage : Maybe String
    , task : TaskDict
    , worker : WorkerDict
    , domain : DomainDict
    , service : ServiceDict
    , doRefresh : Bool
    , urlPrefix : String
    , tableLayout : Table
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
    | Completed Action
    | Uncompleted Action
    | Kill
    | Shutdown


type Msg
    = OnDelete Int
    | NoOperation
    | OnAbort Int
    | OnRelaunch Int
    | GotTasks (Result Http.Error (List Task))
    | GotBool Table Int Action (Result Http.Error Bool)
    | RelaunchMsg Int (Result Http.Error Int)
    | DoRefresh Bool
    | OnRefresh
    | Table String
    | GotServices (Result Http.Error (List Service))
    | GotMonitors (Result Http.Error JsonMonitors)
    | OnKill Int
    | OnShutdown Int


type Table
    = TableTasks
    | TableMonitors
    | TableServices
