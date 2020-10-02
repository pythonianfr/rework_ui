module Type exposing (..)

import AssocList as AL
import Http
import Keyboard.Event exposing (KeyboardEvent)
import List.Selection as LS
import Log exposing (LogEntry, Level)
import Metadata as M


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


type SpecType
    = Num
    | Str
    | File


type alias InputSpec =
    { spectype : SpecType
    , name : String
    , required : Bool
    , choices : List String
    }


type alias Launcher =
    { id : Int
    , operation : String
    , domain : String
    , host : String
    , inputs : List InputSpec
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


type alias WorkerDict =
    AL.Dict Int Worker


type alias MonitorDict =
    AL.Dict Int Monitor


type alias TaskDict =
    AL.Dict Int Task


type alias ServiceDict =
    AL.Dict Int Service


type alias LauncherDict =
    AL.Dict Int Launcher


type alias Model =
    { baseurl : String
    , tasks : TaskDict
    , workers : WorkerDict
    , monitors : MonitorDict
    , services : ServiceDict
    , launchers : LauncherDict
    , launching : Maybe Int
    , activetab : TabsLayout
    , domain : LS.Selection String
    , lasteventid : Int
    -- logging
    , loglevel : Level
    , logdisplaylevel : Level
    , log : List LogEntry
    , logview : Bool
    }


type alias Flags =
    { baseurl : String
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
    | Delete
    | Relaunch
    | Kill
    | Shutdown
    | Disabled Action


type Msg
    = OnDelete Int
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
    | GotLaunchers (Result Http.Error (List Launcher))
    | GotWorkers (Result Http.Error JsonMonitors)
    | OnKill Int
    | OnShutdown Int
    | SetDomain String
    -- launcher
    | OpenForm Int
    | CloseForm
    -- events
    | HandleKeyboardEvent KeyboardEvent
    | SelectDisplayLevel Level


type TabsLayout
    = TasksTab
    | MonitorsTab
    | ServicesTab
    | LauncherTab
