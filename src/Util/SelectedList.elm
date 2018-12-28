module Util.SelectedList
    exposing
        ( SelectedList
        , append
        , index
        , left
        , leftWithDefault
        , leftWithEndpoint
        , map
        , right
        , rightWithDefault
        , rightWithEndpoint
        , set
        , singleton
        , toList
        , triMap
        , value
        , window
        )

import Maybe exposing (..)


type SelectedList a
    = SelectedList ( List a, a, List a )


singleton : a -> SelectedList a
singleton x =
    SelectedList ( [], x, [] )


left : SelectedList a -> Maybe (SelectedList a)
left (SelectedList ( l, x, r )) =
    case l of
        h :: tl ->
            Just <| SelectedList ( tl, h, x :: r )

        _ ->
            Nothing


leftWithDefault : a -> SelectedList a -> SelectedList a
leftWithDefault v ((SelectedList ( l, x, r )) as sl) =
    Maybe.withDefault (SelectedList ( [], v, x :: r )) <| left sl


leftWithEndpoint : SelectedList a -> SelectedList a
leftWithEndpoint sl =
    Maybe.withDefault sl <| left sl


right : SelectedList a -> Maybe (SelectedList a)
right (SelectedList ( l, x, r )) =
    case r of
        h :: tl ->
            Just <| SelectedList ( x :: l, h, tl )

        _ ->
            Nothing


rightWithDefault : a -> SelectedList a -> SelectedList a
rightWithDefault v ((SelectedList ( l, x, r )) as sl) =
    Maybe.withDefault (SelectedList ( x :: l, v, [] )) <| right sl


rightWithEndpoint : SelectedList a -> SelectedList a
rightWithEndpoint sl =
    Maybe.withDefault sl <| right sl


value : SelectedList a -> a
value (SelectedList ( _, x, _ )) =
    x


set : a -> SelectedList a -> SelectedList a
set v (SelectedList ( l, _, r )) =
    SelectedList ( l, v, r )


append : List a -> SelectedList a -> SelectedList a
append lst (SelectedList ( l, x, r )) =
    SelectedList ( l, x, r ++ lst )


toList : SelectedList a -> List a
toList (SelectedList ( l, x, r )) =
    List.reverse l ++ [ x ] ++ r


map : (a -> b) -> SelectedList a -> SelectedList b
map f (SelectedList ( l, x, r )) =
    SelectedList ( List.map f l, f x, List.map f r )


triMap : (a -> b) -> (a -> b) -> (a -> b) -> SelectedList a -> SelectedList b
triMap lf xf rf (SelectedList ( l, x, r )) =
    SelectedList ( List.map lf l, xf x, List.map rf r )


index : SelectedList a -> Int
index (SelectedList ( l, _, _ )) =
    List.length l


window : Int -> SelectedList a -> ( List a, a, List a )
window amount (SelectedList ( l, x, r )) =
    ( List.reverse <| List.take amount l, x, List.take amount r )
