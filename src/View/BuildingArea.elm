module View.BuildingArea exposing (view)

import Game.Constants as Constants
import Game.Level as Level exposing (Level)
import Html exposing (Html)
import Html.Attributes as Html exposing (style)
import Html.Events as Html
import Json.Decode as Json
import Turing.State as State exposing (NonterminalState, State(..))
import Turing.TransitionGrid as TransitionGrid exposing (TransitionGrid)
import Turing.Turing as Turing exposing (Configuration, Direction(..), Output, Turing)
import Turing.Word as Word exposing (Letter, Word)
import Types exposing (..)
import Util.Grid as Grid exposing (Grid)


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.on "keydown" (Json.map tagger Html.keyCode)


stateDisplay state =
    case state of
        Nonterminal i ->
            String.fromInt <| State.toInt i

        Accept ->
            "A"

        Reject ->
            "R"


stateParse int =
    case int of
        65 ->
            -- 'A'
            Just Accept

        82 ->
            -- 'R'
            Just Reject

        ch ->
            (ch - 48)
                |> (\x ->
                        if x < 0 then
                            Nothing

                        else
                            Just <| Nonterminal <| State.fromInt x
                   )


letterDisplay letter =
    String.fromChar <| Word.toChar letter


letterParse int =
    case int of
        189 ->
            -- '-', '_'
            Just Word.blank

        ch ->
            (ch - 65)
                |> (\x ->
                        if x < 0 then
                            Nothing

                        else
                            Just <| Word.fromInt x
                   )


directionDisplay dir =
    case dir of
        Left ->
            "L"

        Right ->
            "R"


directionParse int =
    case int of
        76 ->
            -- 'L'
            Just Left

        82 ->
            -- 'R'
            Just Right

        _ ->
            Nothing


gridDisplay : Grid (Html Msg) -> Html Msg
gridDisplay grid =
    let
        defaultText x y =
            case ( x, y ) of
                ( 0, 0 ) ->
                    ""

                ( 0, _ ) ->
                    letterDisplay <| TransitionGrid.letterFromInt <| y - 1

                ( _, 0 ) ->
                    String.fromInt <| x - 1

                _ ->
                    "ERROR"

        defaultHtml x y =
            Html.input [ Html.class "grid-cell", Html.disabled True, Html.value <| defaultText x y ] []

        func x y =
            Grid.lookup (x - 1) (y - 1) grid
                |> Maybe.withDefault (defaultHtml x y)

        borderedGrid =
            Grid.initialize (Grid.width grid + 1) (Grid.height grid + 1) func
    in
    borderedGrid
        |> Grid.foldRightBottom (::) (\x acc -> Html.div [ Html.class "row" ] x :: acc) [] []
        |> Html.div [ Html.class "table" ]


titled : String -> Html Msg -> Html Msg
titled str html =
    let
        title =
            Html.h1 [] [ Html.text str ]
    in
    Html.div [ Html.class "title" ] [ title, html ]


makeBuildingArea : TransitionGrid -> Bool -> Bool -> Maybe ( NonterminalState, Letter ) -> Html Msg
makeBuildingArea outputGrid allowedLetters allowedDirections active =
    let
        inputAttribute msg parse ch =
            case parse ch of
                Nothing ->
                    NoOp

                Just x ->
                    msg x

        disabledAttribute b =
            Html.disabled b

        classAttribute activeSquare ntstate letter =
            if active == Just ( ntstate, letter ) then
                Html.class "active grid-cell"

            else
                Html.class "grid-cell"

        valueAttribute toString value =
            Html.value <| toString value

        attributes disabled toString msg parse ntstate letter value =
            [ onKeyUp <| inputAttribute (msg ntstate letter) parse
            , disabledAttribute disabled
            , classAttribute active ntstate letter
            , valueAttribute toString value
            ]

        cell disabled toString msg parse ntstate letter value =
            Html.input
                (attributes disabled toString msg parse ntstate letter value)
                []

        makeGrid str disabled toString msg parse getter =
            TransitionGrid.toGrid (cell disabled (toString << getter) msg parse) outputGrid
                |> gridDisplay
                |> titled str
    in
    Html.div
        [ Html.class "tables" ]
        [ makeGrid "Next State" False stateDisplay SetState stateParse .state
        , makeGrid "Move Direction" (not allowedDirections) directionDisplay SetDirection directionParse .direction
        , makeGrid "Write Letter" (not allowedLetters) letterDisplay SetLetter letterParse .letter
        ]


view : Model -> Html Msg
view model =
    let
        active =
            case model.mode of
                BuildingMode ->
                    Nothing

                RunningMode { configuration } ->
                    case Turing.getState configuration of
                        Nonterminal st ->
                            Just ( st, Turing.getSelectedCell configuration )

                        _ ->
                            Nothing
    in
    makeBuildingArea
        model.outputGrid
        model.level.allowedLetters
        model.level.allowedDirections
        active
