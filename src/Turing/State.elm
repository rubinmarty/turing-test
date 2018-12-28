module Turing.State
    exposing
        ( NonterminalState
        , State(..)
        , fromInt
        , initialState
        , isTerminal
        , toInt
        )


type NonterminalState
    = NonterminalState Int


type State
    = Accept
    | Reject
    | Nonterminal NonterminalState


initialState : State
initialState =
    Nonterminal <| NonterminalState 0


isTerminal : State -> Bool
isTerminal state =
    case state of
        Nonterminal _ ->
            False

        _ ->
            True


toInt : NonterminalState -> Int
toInt (NonterminalState x) =
    x


fromInt : Int -> NonterminalState
fromInt =
    NonterminalState
