
import Html exposing (Html, Attribute, div, text, input)
import Html.App exposing (program)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Time exposing (..)
import AnimationFrame
import Random exposing (..)
import Window
import Task

main : Program Never
main =
    program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-------- Init -------------------------
initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, getWindowSizeCommand )


-------- Commands -------------------------
getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Task.perform (always NoOp) WindowSize Window.size


-------- Subscriptions -------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes WindowSize
        ]

-------- Model -------------------------
velocity : Float
velocity = 1.01

starCount : Int
starCount = 100

type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }

bounds: Bounds
bounds =
    { minX = 0.3
    , minY = 0.3
    , maxX = 0.5
    , maxY = 0.5
    }

type alias Star =
    { x: Float
    , y: Float
    }


type alias Model =
    { stars : List Star
    , seed : Seed
    , windowDimensions : Window.Size
    }


defaultModel : Model
defaultModel =
    { stars = []
    , seed = (initialSeed 1337)
    , windowDimensions = { width = 640, height = 480 }
    }


-------- Messages -------------------------
type Msg
    = NoOp
    | WindowSize Window.Size
    | Tick Time.Time



-------- Update -------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                WindowSize newSize ->
                    ( { model | windowDimensions = newSize }, Cmd.none )

                Tick dt ->
                    let
                        movedStars = List.map moveStar model.stars
                        insideField = List.filter filterInside movedStars
                        (updatedStars, updatedSeed) = addStars insideField model.seed
                        model' =  { model |
                                stars = updatedStars,
                                seed = updatedSeed
                            }
                    in
                        (model', Cmd.none)
    in
        ( newModel, cmds )

tupleToStar : (Float, Float) -> Star
tupleToStar (x, y) =
    { x = x, y = y }


addStars : List Star -> Seed -> (List Star, Seed)
addStars stars seed =
    if (starCount - List.length stars) == 0 then
        (stars, seed)
    else
        let
            (star, newSeed) = (generateStar bounds.minX bounds.minY seed)
        in
            addStars (star :: stars) newSeed


moveStar : Star -> Star
moveStar star =
    { star |
        x = star.x * velocity,
        y = star.y * velocity
    }


filterInside : Star -> Bool
filterInside star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxX


generateStar : Float -> Float -> Seed -> (Star, Seed)
generateStar width height seed =
    let
        pair = Random.pair (Random.float -width width) (Random.float -height height)
        (coords, newSeed) = Random.step pair seed
    in
        (tupleToStar coords, newSeed)


-------- View -------------------------
view : Model -> Html Msg
view model =
    let
        { width, height } = model.windowDimensions
        width' = toFloat width
        height' = toFloat height
        forms = List.map (starToForm (width', height')) model.stars
    in
        collage width height ([(background width' height')] ++ forms)
        |> Element.toHtml

background : Float -> Float -> Form
background width height =
    filled black (rect width height)


starToForm : (Float, Float) -> Star -> Form
starToForm (width, height) star =
    move (width * star.x, height * star.y) (filled white (square 3))

