module Turing.TransitionGrid
    exposing
        ( TransitionGrid
        , get
        , initialize
        , letterFromInt
        , letterToInt
        , setDirection
        , setLetter
        , setState
        , toGrid
        , toTransitionFunction
        )

import Turing.State as State exposing (NonterminalState, State(..))
import Turing.Turing as Turing exposing (Direction(..), Output, TransitionFunction, Turing)
import Turing.Word as Word exposing (Letter, Word)
import Util.Grid as Grid exposing (Grid)


type TransitionGrid
    = TransitionGrid (Grid Output)


initialize : Int -> Int -> TransitionGrid
initialize numberOfStates numberOfLetters =
    let
        f x y =
            { state = Nonterminal <| State.fromInt x
            , letter = letterFromInt y
            , direction = Right
            }
    in
    TransitionGrid <| Grid.initialize numberOfStates (numberOfLetters + 1) f


update : (Output -> Output) -> NonterminalState -> Letter -> TransitionGrid -> TransitionGrid
update func state letter (TransitionGrid grid) =
    Grid.update (State.toInt state) (letterToInt letter) func grid
        |> TransitionGrid


setState : State -> NonterminalState -> Letter -> TransitionGrid -> TransitionGrid
setState newState =
    update (\output -> { output | state = newState })


setDirection : Direction -> NonterminalState -> Letter -> TransitionGrid -> TransitionGrid
setDirection newDirection =
    update (\output -> { output | direction = newDirection })


setLetter : Letter -> NonterminalState -> Letter -> TransitionGrid -> TransitionGrid
setLetter newLetter =
    update (\output -> { output | letter = newLetter })


letterFromInt : Int -> Letter
letterFromInt x =
    case x of
        0 ->
            Word.blank

        _ ->
            Word.fromInt <| x - 1


letterToInt : Letter -> Int
letterToInt letter =
    case Word.toMaybeInt letter of
        Nothing ->
            0

        Just x ->
            x + 1


get : NonterminalState -> Letter -> TransitionGrid -> Maybe Output
get state letter (TransitionGrid grid) =
    Grid.lookup (State.toInt state) (letterToInt letter) grid


toTransitionFunction : TransitionGrid -> TransitionFunction
toTransitionFunction transitionGrid state letter =
    get state letter transitionGrid


toGrid : (NonterminalState -> Letter -> Output -> a) -> TransitionGrid -> Grid a
toGrid func (TransitionGrid grid) =
    let
        func2 x y =
            func (State.fromInt x) (letterFromInt y)
    in
    Grid.indexedMap func2 grid
