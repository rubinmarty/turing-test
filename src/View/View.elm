module View.View exposing (view)

import Game.Constants as Constants
import Game.Level as Level exposing (Level)
import Html exposing (Html)
import Html.Attributes as Html exposing (style)
import Html.Events as Html
import Turing.State as State exposing (State(..))
import Turing.TransitionGrid as TransitionGrid exposing (TransitionGrid)
import Turing.Turing as Turing exposing (Configuration, Direction(..), Output, Turing)
import Turing.Word as Word exposing (Letter, Word)
import Types exposing (..)
import Util.Grid as Grid exposing (Grid)
import View.BuildingArea


wordDisplay : Word -> String
wordDisplay word =
    Word.letters word
        |> List.map (String.fromChar << Word.toChar)
        |> String.join ""
        |> (\s -> "\"" ++ s ++ "\"")


wordElement : Maybe Word -> Html Msg
wordElement word =
    let
        text =
            word
                |> Maybe.map (\w -> "Word: " ++ wordDisplay w)
                |> Maybe.withDefault (String.fromChar <| Char.fromCode 160)
    in
    Html.p [] [ Html.text text ]


stateElement : State -> Html Msg
stateElement state =
    let
        str =
            case state of
                Nonterminal i ->
                    String.fromInt <| State.toInt i

                Accept ->
                    "Accept"

                Reject ->
                    "Reject"
    in
    Html.p [] [ Html.text <| "State: " ++ str ]


tapeElement : Configuration -> Html Msg
tapeElement configuration =
    let
        ( l, x, r ) =
            Turing.window 9 configuration

        lpad default total lst =
            List.repeat (total - List.length lst) default ++ lst

        rpad default total lst =
            lst ++ List.repeat (total - List.length lst) default

        left =
            Word.letters l
                |> List.map Just
                |> lpad Nothing 9
                |> List.map (tapeCell False)

        right =
            Word.letters r
                |> rpad Word.blank 9
                |> List.map Just
                |> List.map (tapeCell False)

        center =
            tapeCell True <| Just x

        class selected =
            if selected then
                "selected tape-cell"

            else
                "tape-cell"

        tapeCell selected mletter =
            case mletter of
                Just letter ->
                    Html.span
                        [ Html.class <| class selected ]
                        [ Html.text <| String.fromChar <| Word.toChar letter ]

                Nothing ->
                    Html.span
                        [ Html.class "empty tape-cell" ]
                        [ Html.text <| String.fromChar <| Char.fromCode 160 ]
    in
    Html.p [ Html.class "tape" ] (left ++ [ center ] ++ right)


makeTuringElement : Maybe Word -> Configuration -> Html Msg
makeTuringElement targetWord configuration =
    Html.div
        [ Html.class "turing-row" ]
        [ wordElement targetWord
        , stateElement <| Turing.getState configuration
        , tapeElement configuration
        ]


popup : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
popup attrs htmls =
    let
        closeButton =
            squareButtonWithClasses [ "close-help" ] (SetPopup NoPopup) "X"
    in
    Html.div (Html.class "popup" :: attrs) (closeButton :: htmls)


squareButton : Msg -> String -> Html Msg
squareButton =
    squareButtonWithClasses []


squareButtonWithClasses : List String -> Msg -> String -> Html Msg
squareButtonWithClasses classes msg str =
    let
        attributes =
            [ Html.onClick msg, Html.class "square" ] ++ List.map Html.class classes
    in
    Html.button attributes [ Html.text str ]


startButton : Html Msg
startButton =
    squareButton Start "|>"


speedUpButton : Html Msg
speedUpButton =
    squareButton SpeedUp "+"


speedDownButton : Html Msg
speedDownButton =
    squareButton SpeedDown "-"


pauseButton : Html Msg
pauseButton =
    squareButton Pause "||"


stopButton : Html Msg
stopButton =
    squareButton Stop "X"


makeExamples : List Word -> (Word -> Bool) -> Html Msg
makeExamples words testingFunction =
    let
        ( good, bad ) =
            List.partition testingFunction words

        listExamples lst =
            Html.ul [] <| List.map (\w -> Html.li [] [ Html.text <| wordDisplay w ]) lst

        examples str wds =
            Html.div
                [ Html.class "example-list" ]
                [ Html.h3 [] [ Html.text str ]
                , listExamples wds
                ]
    in
    Html.div [ Html.class "examples" ] [ examples "Accept these: " good, examples "Reject these: " bad ]


makePopup : Popup -> Html Msg
makePopup ppp =
    case ppp of
        HelpPopup ->
            popup [] (List.map (\s -> Html.p [] [ Html.text s ]) Constants.helpMessage)

        LevelSelectorPopup ->
            let
                text =
                    Html.p [] [ Html.text "Select a level to skip to:" ]

                makeButton lvl =
                    squareButton (SkipToLevel lvl) (String.fromInt lvl.levelNumber)

                buttons =
                    List.map makeButton Level.levels
            in
            popup [] (text :: buttons)

        NoPopup ->
            Html.text ""


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model: " model

        levelCount =
            Html.h1 [] [ Html.text <| "Level " ++ String.fromInt model.level.levelNumber ]

        description =
            Html.span [] [ Html.text <| model.level.description ]

        examples =
            makeExamples model.level.words model.level.testingFunction

        info =
            Html.div [ Html.class "info" ] [ description, examples ]

        upperButtons =
            Html.div
                [ Html.class "button-row"
                ]
                [ startButton
                , pauseButton
                , speedDownButton
                , speedUpButton
                , stopButton
                ]

        turingElement =
            case model.mode of
                BuildingMode ->
                    makeTuringElement Nothing (Turing.configuration Word.empty)

                RunningMode { configuration, word } ->
                    makeTuringElement
                        (Just word)
                        configuration

        buildingArea =
            View.BuildingArea.view model

        body =
            Html.div [ Html.class "everything" ] [ levelCount, info, upperButtons, turingElement, buildingArea ]

        menuButtons =
            Html.div [ Html.class "menu-buttons" ] [ squareButton (SetPopup LevelSelectorPopup) "...", squareButton (SetPopup HelpPopup) "?" ]

        popupElement =
            makePopup model.popup
    in
    Html.div [] [ menuButtons, body, popupElement ]
