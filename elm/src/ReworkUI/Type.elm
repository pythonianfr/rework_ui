module ReworkUI.Type exposing (..)

import AssocList as AL
import Http
import List.Selection as LS
import ReworkUI.Metadata as M


type alias Task =
    { id : Int
    , result : TaskResult
    , name : String
    , domain : String
    -- the 3 following are timestamps
    , queued : String
    , started : Maybe String
    , finished : Maybe String
    , metadata : Maybe M.Metadata
    , worker : Maybe Int
    , status : Status
    , deathInfo : Maybe String
    , actions : List Action
    }


type alias Event =
    { id : Int
    , action : String
    , taskid : Int
    }


type alias Service =
    { id : Int
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
    { id : Int
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
    { errors : List String
    , tasks : TaskDict
    , workers : WorkerDict
    , monitors : MonitorDict
    , services : ServiceDict
    , urlPrefix : String
    , activetab : TabsLayout
    , domain : LS.Selection String
    , lasteventid : Int
    }


type alias Flags =
    { urlPrefix : String
    , domains : List String
    }


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
    | GotTasks (Result Http.Error String)
    | UpdatedTasks (Result Http.Error String)
    | GotEvents (Result Http.Error String)
    | GotLastEvent (Result Http.Error String)
    | ActionResponse TabsLayout Int Action (Result Http.Error Bool)
    | RelaunchMsg Int (Result Http.Error Int)
    | OnRefresh
    | Tab TabsLayout
    | GotServices (Result Http.Error (List Service))
    | GotWorkers (Result Http.Error JsonMonitors)
    | OnKill Int
    | OnShutdown Int
    | SetDomain String


type TabsLayout
    = TasksTab
    | MonitorsTab
    | ServicesTab
