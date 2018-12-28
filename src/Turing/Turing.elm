module Turing.Turing
    exposing
        ( Behavior(..)
        , Configuration
        , Direction(..)
        , Output
        , TransitionFunction
        , Turing
        , behavior
        , configuration
        , getSelectedCell
        , getState
        , index
        , step
        , tapeContents
        , turing
        , window
        )

import Turing.State as State exposing (NonterminalState, State(..))
import Turing.Word as Word exposing (Letter, Word)
import Util.Grid as Grid exposing (Grid)
import Util.SelectedList as SelectedList exposing (..)


type Direction
    = Left
    | Right


type alias Output =
    { state : State
    , letter : Letter
    , direction : Direction
    }


type alias TransitionFunction =
    NonterminalState -> Letter -> Maybe Output


type Turing
    = Turing
        { alphabetSize : Int
        , nonterminalStateSize : Int
        , transition : TransitionFunction
        }


type Configuration
    = Configuration
        { tape : SelectedList Letter
        , state : State
        }


window : Int -> Configuration -> ( Word, Letter, Word )
window amount (Configuration { tape }) =
    let
        ( l, x, r ) =
            SelectedList.window amount tape
    in
    ( Word.fromLetters l, x, Word.fromLetters r )


getState : Configuration -> State
getState (Configuration config) =
    config.state


getSelectedCell : Configuration -> Letter
getSelectedCell (Configuration config) =
    SelectedList.value config.tape


turing : Int -> Int -> TransitionFunction -> Turing
turing alphabetSize nonterminalStateSize transition =
    Turing
        { alphabetSize = alphabetSize
        , nonterminalStateSize = nonterminalStateSize
        , transition = transition
        }


configuration : Word -> Configuration
configuration word =
    let
        tape =
            case Word.uncons word of
                Nothing ->
                    SelectedList.singleton Word.blank

                Just ( first, tail ) ->
                    SelectedList.singleton first
                        |> SelectedList.append (Word.letters tail)
    in
    Configuration
        { tape = tape
        , state = State.initialState
        }


tapeContents : Configuration -> List Letter
tapeContents (Configuration config) =
    SelectedList.toList config.tape


index : Configuration -> Int
index (Configuration config) =
    SelectedList.index config.tape


write : Letter -> Configuration -> Configuration
write letter (Configuration config) =
    Configuration { config | tape = SelectedList.set letter config.tape }


shiftDirection : Direction -> Configuration -> Maybe Configuration
shiftDirection direction (Configuration config) =
    case direction of
        Left ->
            SelectedList.left config.tape
                |> Maybe.map (\tape -> Configuration { config | tape = tape })

        Right ->
            Just <| Configuration { config | tape = SelectedList.rightWithDefault Word.blank config.tape }


update : Output -> Configuration -> Maybe Configuration
update output (Configuration config) =
    Configuration { config | state = output.state }
        |> write output.letter
        |> shiftDirection output.direction


isTerminal : Configuration -> Bool
isTerminal (Configuration config) =
    State.isTerminal config.state


validate : Turing -> Configuration -> Bool
validate (Turing t) (Configuration { tape, state }) =
    let
        validState =
            case state of
                Nonterminal nts ->
                    State.toInt nts >= 0 && State.toInt nts < t.nonterminalStateSize

                _ ->
                    True

        validLetter =
            Word.inAlphabetOfSize t.alphabetSize <| SelectedList.value tape
    in
    validState && validLetter


step : Turing -> Configuration -> Configuration
step ((Turing t) as tur) (Configuration config) =
    case config.state of
        Nonterminal state ->
            let
                output =
                    t.transition state (SelectedList.value config.tape)

                defaultConfig =
                    Configuration { config | state = Reject }

                returnConfig =
                    output
                        |> Maybe.andThen (\out -> update out (Configuration config))
                        |> Maybe.andThen
                            (\c ->
                                if validate tur c then
                                    Just c

                                else
                                    Nothing
                            )
                        |> Maybe.withDefault defaultConfig
            in
            returnConfig

        _ ->
            Configuration config


type Behavior
    = Success
    | Failure
    | Timeout


run : Int -> Turing -> Configuration -> Configuration
run times tur config =
    if times == 0 || isTerminal config then
        config

    else
        run (times - 1) tur (step tur config)


testTuring : Int -> List Word -> (Word -> Bool) -> Turing -> Bool
testTuring timeout words pred tur =
    let
        a =
            List.map pred words

        b =
            List.map (behavior timeout tur) words

        c =
            List.map2 (\i j -> ( i, j )) a b

        same ( x, y ) =
            (x && (y == Success)) || (not x && (y == Failure))
    in
    List.all same c


behavior : Int -> Turing -> Word -> Behavior
behavior timeout tur word =
    let
        (Configuration c) =
            run timeout tur (configuration word)
    in
    case c.state of
        Accept ->
            Success

        Reject ->
            Failure

        Nonterminal _ ->
            Timeout
