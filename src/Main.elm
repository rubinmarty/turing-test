module Main exposing (..)

import Browser
import Game.Level as Level exposing (Level)
import Random exposing (Generator)
import Time
import Turing.State as State exposing (State(..))
import Turing.TransitionGrid as TransitionGrid exposing (TransitionGrid)
import Turing.Turing as Turing exposing (Behavior(..), Direction(..), Output, Turing)
import Turing.Word as Word exposing (Word)
import Types exposing (..)
import Util.Grid as Grid exposing (Grid)
import View.View as View


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = View.view
        }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( loadLevel <| Level.getLevel 1, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( SetPopup p, _ ) ->
            ( { model | popup = p }, Cmd.none )

        ( SkipToLevel level, _ ) ->
            ( loadLevel level, Cmd.none )

        ( Start, _ ) ->
            ( start model, Cmd.none )

        ( Tick, RunningMode runningData ) ->
            ( tick runningData model, Cmd.none )

        ( Pause, RunningMode runningData ) ->
            ( { model | mode = RunningMode { runningData | running = False } }, Cmd.none )

        ( SpeedUp, _ ) ->
            ( { model | speed = model.speed + 1 }, Cmd.none )

        ( SpeedDown, _ ) ->
            ( { model | speed = max 1 <| model.speed - 1 }, Cmd.none )

        ( Stop, RunningMode _ ) ->
            ( stop model, Cmd.none )

        ( Receive words, _ ) ->
            ( receive words model, Cmd.none )

        ( SetState ntstate letter state, _ ) ->
            ( setState ntstate letter state model, Cmd.none )

        ( SetLetter ntstate letter lett, _ ) ->
            ( setLetter ntstate letter lett model, Cmd.none )

        ( SetDirection ntstate letter direction, _ ) ->
            ( setDirection ntstate letter direction model, Cmd.none )

        _ ->
            ( model, Cmd.none )


type WordStatus
    = PassingWord
    | FailingWord
    | UnsureWord


wordStatus : Word -> (Word -> Bool) -> Turing -> WordStatus
wordStatus word pred turing =
    let
        timeout =
            1000

        expected =
            pred word

        actual =
            Turing.behavior timeout turing word
    in
    case ( actual, expected ) of
        ( Timeout, _ ) ->
            UnsureWord

        ( Success, True ) ->
            PassingWord

        ( Failure, False ) ->
            PassingWord

        _ ->
            FailingWord


findToughestWord : List Word -> (Word -> Bool) -> Turing -> Word
findToughestWord words pred turing =
    let
        helper safety wds =
            case wds of
                w :: ws ->
                    case wordStatus w pred turing of
                        FailingWord ->
                            w

                        PassingWord ->
                            helper safety ws

                        UnsureWord ->
                            helper w ws

                [] ->
                    safety
    in
    helper (Maybe.withDefault Word.empty <| List.head words) words


receive : List Word -> Model -> Model
receive words model =
    case model.mode of
        RunningMode ({ turing } as runningData) ->
            let
                toughWord =
                    findToughestWord words model.level.testingFunction turing

                newMode =
                    RunningMode { runningData | word = toughWord }
            in
            { model | mode = newMode }

        _ ->
            model


loadLevel : Level -> Model
loadLevel level =
    let
        numberOfStates =
            level.numberOfStates

        numberOfLetters =
            level.numberOfLetters
    in
    { outputGrid = TransitionGrid.initialize numberOfStates numberOfLetters
    , mode = BuildingMode
    , level = level
    , speed = 1
    , popup = NoPopup
    }


start : Model -> Model
start model =
    let
        transition =
            TransitionGrid.toTransitionFunction
                model.outputGrid

        turing =
            Turing.turing
                model.level.numberOfLetters
                model.level.numberOfStates
                transition

        word =
            findToughestWord model.level.words model.level.testingFunction turing

        configuration =
            Turing.configuration word

        newRunningData =
            { configuration = configuration
            , turing = turing
            , running = True
            , word = word
            }
    in
    case model.mode of
        BuildingMode ->
            { model | mode = RunningMode newRunningData }

        RunningMode runningData ->
            if State.isTerminal <| Turing.getState runningData.configuration then
                { model | mode = RunningMode newRunningData }

            else
                tick { runningData | running = True } model


stop : Model -> Model
stop model =
    { model | mode = BuildingMode }


nextLevel : Model -> Model
nextLevel model =
    loadLevel <| Level.getLevel (model.level.levelNumber + 1)


tick : RunningData -> Model -> Model
tick ({ configuration, turing, word } as runningData) model =
    case Turing.getState configuration of
        Accept ->
            if model.level.testingFunction word then
                nextLevel model

            else
                model

        Reject ->
            if not <| model.level.testingFunction word then
                nextLevel model

            else
                model

        Nonterminal _ ->
            { model | mode = RunningMode { runningData | configuration = Turing.step turing configuration } }


setField validator value updater model =
    if validator value then
        { model | outputGrid = updater value model.outputGrid }

    else
        model


setState ntstate letter state model =
    if
        case state of
            Nonterminal st ->
                State.toInt st >= 0 && State.toInt st < model.level.numberOfStates

            _ ->
                True
    then
        { model | outputGrid = TransitionGrid.setState state ntstate letter model.outputGrid }

    else
        model


setLetter ntstate letter lett model =
    if
        model.level.allowedLetters
            && Word.inAlphabetOfSize model.level.numberOfLetters letter
    then
        { model | outputGrid = TransitionGrid.setLetter lett ntstate letter model.outputGrid }

    else
        model


setDirection ntstate letter direction model =
    if model.level.allowedDirections then
        { model | outputGrid = TransitionGrid.setDirection direction ntstate letter model.outputGrid }

    else
        model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        RunningMode { running } ->
            case running of
                True ->
                    Time.every (1000 / toFloat model.speed) (always Tick)

                False ->
                    Sub.none

        _ ->
            Sub.none
