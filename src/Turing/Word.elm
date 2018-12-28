module Turing.Word
    exposing
        ( Letter
        , Word
        , blank
        , empty
        , fromInt
        , fromInts
        , fromLetters
        , inAlphabetOfSize
        , letters
        , toChar
        , toMaybeInt
        , uncons
        )


type Letter
    = Simply Int
    | Blank


type Word
    = Word (List Letter)


empty : Word
empty =
    Word []


blank : Letter
blank =
    Blank


inAlphabetOfSize : Int -> Letter -> Bool
inAlphabetOfSize size letter =
    case letter of
        Blank ->
            True

        Simply l ->
            0 <= l && l < size


toMaybeInt : Letter -> Maybe Int
toMaybeInt letter =
    case letter of
        Blank ->
            Nothing

        Simply x ->
            Just x


toChar : Letter -> Char
toChar letter =
    case letter of
        Blank ->
            '_'

        Simply lett ->
            Char.fromCode <| lett + 97


uncons : Word -> Maybe ( Letter, Word )
uncons (Word lst) =
    case lst of
        h :: tl ->
            Just ( h, Word tl )

        [] ->
            Nothing


letters : Word -> List Letter
letters (Word lst) =
    lst


fromLetters : List Letter -> Word
fromLetters =
    Word


fromInt : Int -> Letter
fromInt =
    Simply


fromInts : List Int -> Word
fromInts lst =
    Word <| List.map Simply lst
