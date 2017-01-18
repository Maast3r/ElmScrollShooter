import Html exposing (Html, text, div)
import Mouse exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import List

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = {
    x : Int
  , y : Int
  , r : Int
  , fill : String
  , borders : Borders
  , stars : List Star
}

init : (Model, Cmd Msg)
init =
  ({x=30,
    y=440,
    r=20,
    fill="#AAFFFF",
    borders=gameBorders,
    stars=starList}, Cmd.none)

type alias Borders = {left : Int, right : Int, top : Int, bottom : Int, fill : String}
gameBorders : Borders
gameBorders =
  {
    left=0,
    right=1360,
    top=0,
    bottom=1400,
    fill="#000000"
  }

type alias Star = {x : Int, y : Int}
starList : List Star
starList = 
  [
    {x=0, y=155}
  , {x=50, y=22}
  , {x=170, y=856}
  , {x=90, y=234}
  , {x=200, y=120}
  , {x=250, y=68}
  , {x=310, y=1267}
  , {x=320, y=945}
  , {x=450, y=55}
  , {x=400, y=734}
  , {x=500, y=434}
  , {x=600, y=234}
  , {x=570, y=10}
  , {x=610, y=789}
  , {x=870, y=1089}
  , {x=700, y=875}
  , {x=915, y=55}
  , {x=1360, y=734}
  , {x=790, y=434}
  , {x=900, y=234}
  , {x=1350, y=10}
  , {x=1150, y=789}
  , {x=840, y=1089}
  , {x=100, y=875}
  , {x=1350, y=670}
  , {x=1150, y=189}
  , {x=908, y=1089}
  , {x=1079, y=875}
  ]

-- UPDATE

type Msg
  = Move Int Int | Click | MoveStars Time

update: Msg -> Model -> (Model, Cmd a)
update msg model =
  case msg of
    Move x y ->
      ({ model | x = x, y = y } , Cmd.none)
    Click -> (model, Cmd.none)
    MoveStars t ->
      ({ model | stars=(List.map moveStarsHelper model.stars)}, Cmd.none)

moveStarsHelper : Star -> Star
moveStarsHelper star =
  let
    starResetY = 0
    starNextY = star.y+1
  in
    if star.y >= 1400 then
      {star | y = starResetY}
    else
      {star | y = starNextY}

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
   [ Mouse.moves (\{x, y} -> Move x y),
     Mouse.clicks (\{x, y} -> Click),
     Time.every (50*Time.millisecond) MoveStars]

-- VIEW

view : Model -> Html Msg
view model = 
  svg [ viewBox "0 0 100% 100%", width "100%", height "99%" ]
      (
        [rect [ x (toString model.borders.left), y (toString model.borders.top), width (toString model.borders.right),
          height (toString model.borders.bottom), fill model.borders.fill ] []]
      ++ [circle [ cx (toString model.x), cy (toString model.y), r (toString model.r), fill model.fill ] []]
      ++ List.map(\star -> (circle [cx (toString star.x), cy (toString star.y), r "2", fill "#FFFFFF"] [])) model.stars)
    