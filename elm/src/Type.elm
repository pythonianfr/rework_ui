module Type exposing (..)

import AssocList as AL
import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import Http
import Http.Detailed as HD
import InfiniteScroll as IS
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
    , input : Maybe String
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


type alias Plan =
    { id : Int
    , timestamp : String
    , operation : String
    , input : String
    , domain : String
    }


defaultrule = "0 * * * * *"

type alias Scheduler =
    { id : Int
    , service : String
    , domain : String
    , host : String
    , rule : String
    , input : Maybe String
    }


type SpecType
    = Num
    | Bool
    | Str
    | File
    | Datetime
    | Moment


type alias IOSpec =
    { spectype : SpecType
    , name : String
    , required : Bool
    , default : Maybe String
    , choices : Maybe (List String)
    }


type alias Launcher =
    { id : Int
    , operation : String
    , domain : String
    , host : String
    , inputs : List IOSpec
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


type alias SchedulerDict =
    AL.Dict Int Scheduler


type alias TasksFilter =
    { service : Maybe String
    , inputs : Maybe String
    , status : Maybe String
    }


type alias Model =
    { baseurl : String
    , canwrite : Bool
    , tasks : TaskDict
    , tasksfilter : TasksFilter
    , workers : WorkerDict
    , monitors : MonitorDict
    , services : ServiceDict
    , launchers : LauncherDict
    , launching : Maybe Int
    , activetab : TabsLayout
    , domain : LS.Selection String
    -- loading
    , loading : Bool
    , toload : Bool
    , forceload : Bool
    , scroller : IS.Model Msg
    , height : Float
    , lasteventid : Int
    -- single input/output files
    , inputfilehints : Dict String String
    , outputfilehints : Dict String String
    -- logging
    , loglevel : Level
    , logdisplaylevel : Level
    , log : List LogEntry
    , logview : Bool
    -- scheduler
    , schedulers : SchedulerDict
    , selectedservice : Maybe (String, String)
    , selectedhost : Maybe String
    , selectedrule : String
    , lasterror : Maybe String
    -- plan
    , hours : Int
    , events : List Plan
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
    = HttpNoop (Result Http.Error ())
    | Noop
    | OnDelete Int
    | OnAbort Int
    | OnRelaunch Int
    | ForceLoad
    | GotPermissions (Result Http.Error String)
    | GotTasks (Result Http.Error String)
    | GotInputFileHint (Result Http.Error String)
    | GotOutputFileHint (Result Http.Error String)
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
    -- launcher
    | GotLaunchers (Result Http.Error (List Launcher))
    | OpenForm Int
    | CloseForm
    | Schedule String
    | DirectSchedule Launcher
    | NewScheduler
    -- scheduler
    | GotSchedulers (Result Http.Error (List Scheduler))
    | ScheduleService String String
    | ScheduleHost String
    | ScheduleRule String
    | TestedRule (Result (HD.Error String) (Http.Metadata, String))
    | PreSchedule
    | CancelPreSchedule
    | DeleteSched Int
    | DeletedSched (Result Http.Error String)
    | PreScheduleFailed String
    | PreScheduleOk String
    | LaunchNow Int
    | InstantLaunchFromSchedule (Result Http.Error ())
    -- events
    | HandleKeyboardEvent KeyboardEvent
    | SelectDisplayLevel Level
    | LoadMore
    | ScrollMore IS.Msg
    | GotInitialViewport Viewport
    | Resize (Float, Float)
    -- filters
    | ServiceFilter String
    | InputsFilter String
    | StatusFilter String
    -- plans
    | GotPlans (Result Http.Error (List Plan))
    | Hours String


type TabsLayout
    = TasksTab
    | MonitorsTab
    | ServicesTab
    | LaunchersTab
    | SchedulersTab
    | PlansTab
