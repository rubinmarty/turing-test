module Game.Level exposing (Level, firstWord, getLevel, levels)

import Array exposing (Array)
import Random exposing (Generator)
import Turing.Word as Word exposing (Word)


type alias Level =
    { description : String
    , levelNumber : Int
    , numberOfStates : Int
    , numberOfLetters : Int
    , words : List Word
    , wordGenerator : Generator Word
    , testingFunction : Word -> Bool
    , allowedDirections : Bool
    , allowedLetters : Bool
    }


randomWord : Int -> Int -> Generator Word
randomWord alphabetSize maxlength =
    Random.int 0 maxlength
        |> Random.andThen (\r -> Random.list r (Random.int 0 <| alphabetSize - 1))
        |> Random.map Word.fromInts


firstWord : Level -> Word
firstWord level =
    level.words
        |> List.head
        |> Maybe.withDefault Word.empty


level1 =
    { description = "Accept the input word only if it contains an 'a'."
    , levelNumber = 1
    , numberOfStates = 2
    , numberOfLetters = 2
    , words = [ [ 1, 1, 1, 1, 1, 0 ], [ 0 ], [ 1 ], [ 1, 0 ], [ 1, 1, 0, 1, 1 ], [ 0, 1, 0 ], [], [ 0, 0, 0, 0, 1 ], [ 1, 1, 1, 1, 1 ] ]
    , wordGenerator = randomWord 2 8
    , testingFunction = \lst -> List.member (Word.fromInt 0) lst
    , allowedDirections = False
    , allowedLetters = False
    }


level2 =
    { description = "Accept the input word only if it contains the substring 'abc'."
    , levelNumber = 2
    , numberOfStates = 3
    , numberOfLetters = 4
    , words = [ [ 0, 1, 2 ], [ 3, 0, 1, 2, 3 ], [ 0, 2, 1, 2 ], [ 0, 1, 0, 1, 3, 0, 1, 2, 3, 0 ], [], [ 0, 1 ], [ 2, 1, 0 ] ]
    , wordGenerator = randomWord 4 10
    , testingFunction =
        let
            test lst =
                case lst of
                    a :: b :: c :: tl ->
                        ( a, b, c ) == ( Word.fromInt 0, Word.fromInt 1, Word.fromInt 2 ) || test (b :: c :: tl)

                    _ ->
                        False
        in
        test
    , allowedDirections = False
    , allowedLetters = False
    }


level3 =
    { description = "Accept the input word only if it contains an even number of 'a's."
    , levelNumber = 3
    , numberOfStates = 2
    , numberOfLetters = 2
    , words = [ [ 0 ], [ 0, 1 ], [ 1, 0, 1 ], [ 0, 1, 0, 1, 0, 0, 1, 1 ], [], [ 0, 0 ], [ 1, 1, 1 ], [ 1, 0, 1 ] ]
    , wordGenerator = randomWord 4 10
    , testingFunction = \lst -> (==) 0 <| modBy 2 <| List.length (List.filter ((==) <| Word.fromInt 0) lst)
    , allowedDirections = False
    , allowedLetters = False
    }


level4 =
    { description = "Accept the input word only if its last two letters are the same."
    , levelNumber = 4
    , numberOfStates = 6
    , numberOfLetters = 4
    , words = [ [ 0, 0, 0 ], [ 0, 1, 1, 1 ], [ 2, 2, 0, 2, 2 ], [ 0, 0, 1, 1, 1, 3 ], [ 0, 1, 0, 1, 0, 0, 3, 3 ], [], [ 0, 0 ], [ 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 2, 2, 2 ], [ 0, 1, 3, 0, 3 ] ]
    , wordGenerator = randomWord 4 10
    , testingFunction =
        \lst ->
            case List.take 2 <| List.reverse lst of
                x :: y :: [] ->
                    x == y

                _ ->
                    False
    , allowedDirections = True
    , allowedLetters = False
    }


level5 =
    { description = "Accept the input word only if its last three letters are all the same."
    , levelNumber = 5
    , numberOfStates = 6
    , numberOfLetters = 2
    , words = [ [ 0, 0, 0 ], [ 0, 1, 1, 1, 1, 1 ], [ 1, 1, 0, 1, 1 ], [ 0, 0, 1, 1, 1, 0 ], [ 0, 1, 0, 1, 0, 0, 1, 1 ], [], [ 0, 0 ], [ 1 ], [ 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0 ] ]
    , wordGenerator = randomWord 4 10
    , testingFunction =
        \lst ->
            case List.take 3 <| List.reverse lst of
                x :: y :: z :: [] ->
                    x == y && y == z

                _ ->
                    False
    , allowedDirections = True
    , allowedLetters = False
    }


level6 =
    { description = "TODO"
    , levelNumber = 6
    , numberOfStates = 4
    , numberOfLetters = 4
    , words = [ [ 0, 0, 0, 0 ] ]
    , wordGenerator = randomWord 4 10
    , testingFunction =
        \lst -> False
    , allowedDirections = True
    , allowedLetters = True
    }


toLevel lvl =
    { description = lvl.description
    , levelNumber = lvl.levelNumber
    , numberOfStates = lvl.numberOfStates
    , numberOfLetters = lvl.numberOfLetters
    , words = List.map Word.fromInts lvl.words
    , wordGenerator = lvl.wordGenerator
    , testingFunction = lvl.testingFunction << Word.letters
    , allowedDirections = lvl.allowedDirections
    , allowedLetters = lvl.allowedLetters
    }


levels : List Level
levels =
    List.map toLevel
        [ level1
        , level2
        , level3
        , level4
        , level5
        , level6
        ]


levelsArray =
    Array.fromList levels


getLevel : Int -> Level
getLevel x =
    Array.get (x - 1) levelsArray
        |> Maybe.withDefault (toLevel level1)
