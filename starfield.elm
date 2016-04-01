import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Window
import Debug


type alias Star = (Float, Float)

type alias Stars = List Star

stars : Stars
stars = (generateStars 1.0 1.0)


background : Float -> Float -> Form
background w h =
    filled black (rect w h)


starToForm : (Float, Float) -> (Float, Float) -> Form
starToForm (w, h) (x, y) =
    move (w*x, h*y) (filled white (square 3))


moveStar : Float -> Star -> Star
moveStar d star =
    let
        (x, y) = star
        seed = floor d
    in
        if abs x > 1.0 || abs y > 1.0 then
            fst (Random.generate (generatePair 1.0 1.0) (Random.initialSeed seed))
        else
            (x * 1.01, y * 1.01)


generatePair : Float -> Float -> Random.Generator (Float, Float)
generatePair width height =
    Random.pair (Random.float -width width) (Random.float -height height)


generateList : Float -> Float -> (List (Float, Float), Seed)
generateList w h =
    Random.generate (Random.list 200 (generatePair w h)) (Random.initialSeed 1)


generateStars : Float -> Float -> List Star
generateStars w h = 
    (fst (generateList w h))


update : Float -> Stars -> Stars
update time stars =
    List.map (moveStar time) stars --(Debug.log "stars" stars)


view : Stars -> (Int, Int) -> Element
view stars (w, h) =
    let
        w' = toFloat w
        h' = toFloat h
        forms = List.map (starToForm (w', h')) stars
    in
        collage w h ([(background w' h')] ++ forms)


main : Signal Element
main = 
    -- view (update 0 stars) (500, 500)
    Signal.map2 view (Signal.foldp update stars (Signal.foldp (+) 0 (fps 60))) Window.dimensions
