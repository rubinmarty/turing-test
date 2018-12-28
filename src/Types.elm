module Types exposing (..)

import Game.Level as Level exposing (Level)
import Random exposing (Generator)
import Turing.State as State exposing (NonterminalState, State)
import Turing.TransitionGrid exposing (TransitionGrid)
import Turing.Turing as Turing exposing (Configuration, Direction, Output, Turing)
import Turing.Word as Word exposing (Letter, Word)
import Util.Grid as Grid exposing (Grid)


type alias RunningData =
    { configuration : Configuration
    , turing : Turing
    , running : Bool
    , word : Word
    }


type Mode
    = BuildingMode
    | RunningMode RunningData


type Popup
    = NoPopup
    | HelpPopup
    | LevelSelectorPopup


type alias Model =
    { outputGrid : TransitionGrid
    , mode : Mode
    , level : Level
    , speed : Int
    , popup : Popup
    }


type Msg
    = NoOp
    | SetPopup Popup
    | SkipToLevel Level
      --
    | Start
    | Pause
    | Tick
    | SpeedUp
    | SpeedDown
    | Stop
      --
    | Receive (List Word)
      --
    | SetState NonterminalState Letter State
    | SetLetter NonterminalState Letter Letter
    | SetDirection NonterminalState Letter Direction
