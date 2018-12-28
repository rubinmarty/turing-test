module Util.Grid exposing (..)

--import Array.Extra

import Array exposing (Array)
import Maybe exposing (Maybe)


type alias Grid a =
    Array (Array a)


type alias GridFold a b c =
    Combine a b -> Combine b c -> b -> c -> Grid a -> c


type alias ArrayFold a b =
    Combine a b -> b -> Array a -> b


type alias Combine a b =
    a -> b -> b


empty : Grid a
empty =
    Array.empty


grid : Int -> Int -> a -> Grid a
grid x y v =
    v
        |> Array.repeat x
        |> Array.repeat y


initialize : Int -> Int -> (Int -> Int -> a) -> Grid a
initialize w h f =
    let
        uncurry fn ( a, b ) =
            fn a b

        curry fn a b =
            fn ( a, b )

        flip fn a b =
            fn b a
    in
    grid w h 0
        |> indexedMap ((curry << flip << always << uncurry) f)


height : Grid a -> Int
height g =
    Array.length g


width : Grid a -> Int
width g =
    g
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


dimensions : Grid a -> ( Int, Int )
dimensions g =
    ( width g, height g )


lookup : Int -> Int -> Grid a -> Maybe a
lookup x y g =
    g
        |> Array.get y
        |> Maybe.andThen (Array.get x)


set : Int -> Int -> a -> Grid a -> Grid a
set x y val g =
    Array.set y (Array.set x val (Array.get y g |> Maybe.withDefault Array.empty)) g


update : Int -> Int -> (a -> a) -> Grid a -> Grid a
update x y f g =
    lookup x y g
        |> Maybe.map f
        |> Maybe.map (\v -> set x y v g)
        |> Maybe.withDefault g


map : (a -> b) -> Grid a -> Grid b
map f g =
    Array.map (Array.map f) g


indexedMap : (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap f g =
    let
        flip fn y z =
            fn z y
    in
    Array.indexedMap (\y -> Array.indexedMap (flip f y)) g


countIf : (a -> Bool) -> Grid a -> Int
countIf predicate g =
    let
        incrIf x acc =
            if predicate x then
                1 + acc

            else
                acc
    in
    foldLeftTop incrIf (+) 0 0 g


genericFold : ArrayFold a b -> ArrayFold (Array a) c -> GridFold a b c
genericFold foldh foldv combine combineRows base baseRow g =
    let
        foldRow : Array a -> b
        foldRow =
            foldh combine base

        foldAndCombineRow : Combine (Array a) c
        foldAndCombineRow row acc =
            combineRows (foldRow row) acc
    in
    foldv foldAndCombineRow baseRow g


foldLeftTop : GridFold a b c
foldLeftTop =
    genericFold Array.foldl Array.foldl


foldLeftBottom : GridFold a b c
foldLeftBottom =
    genericFold Array.foldl Array.foldr


foldRightBottom : GridFold a b c
foldRightBottom =
    genericFold Array.foldr Array.foldr


foldRightTop : GridFold a b c
foldRightTop =
    genericFold Array.foldr Array.foldl


filter : (a -> Bool) -> Grid a -> Grid (Maybe a)
filter predicate g =
    map
        (\x ->
            if predicate x then
                Just x

            else
                Nothing
        )
        g
