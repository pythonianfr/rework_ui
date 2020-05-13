module ReworkUI.Type exposing (..)

import AssocList as AL
import Http
import List.Selection as LS


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


type alias Monitor =
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
    { monitors : List Monitor
    , workers : List Worker
    }


type alias IntDict a =
    AL.Dict Int a


type alias WorkerDict =
    IntDict Worker


type alias MonitorDict =
    IntDict Monitor


type alias TaskDict =
    IntDict Task


type alias ServiceDict =
    IntDict Service


type alias Model =
    { errorMessage : Maybe String
    , task : TaskDict
    , worker : WorkerDict
    , monitor : MonitorDict
    , service : ServiceDict
    , urlPrefix : String
    , tableLayout : TableLayout
    , userDomain : LS.Selection String
    }


type alias Flags =
    { urlPrefix : String
    , domains : List String
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
    | GotBool TableLayout Int Action (Result Http.Error Bool)
    | RelaunchMsg Int (Result Http.Error Int)
    | OnRefresh
    | Table TableLayout
    | GotServices (Result Http.Error (List Service))
    | GotWorkers (Result Http.Error JsonMonitors)
    | OnKill Int
    | OnShutdown Int
    | SetDomain String


type TableLayout
    = TableTasks
    | TableMonitors
    | TableServices
