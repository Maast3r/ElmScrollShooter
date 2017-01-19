import Html exposing (Html, text, div)
import Mouse exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import List

{- INSTRUCTIONS
  - Use your mouse to move the plane
  - Left click to fire your weapon
  - 1-4 to cycle weapons
-}

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


--- MODEL ---


type alias Model = {
    x : Int
  , y : Int
  , r : Int
  , fill : String
  , borders : Borders
  , stars : List Star
  , bullets : List BulletUpdater
  , enemies : List EnemyUpdater
}

init : (Model, Cmd Msg)
init =
  ({x=30,
    y=440,
    r=20,
    fill="#AAFFFF",
    borders=gameBorders,
    stars=starList,
    bullets=[],
    enemies=[]}, Cmd.none)

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
  , {x=870, y=589}
  , {x=700, y=875}
  , {x=915, y=55}
  , {x=1360, y=734}
  , {x=790, y=434}
  , {x=900, y=234}
  , {x=1350, y=10}
  , {x=1150, y=789}
  , {x=840, y=89}
  , {x=100, y=875}
  , {x=1350, y=670}
  , {x=1150, y=189}
  , {x=908, y=1089}
  , {x=1079, y=875}
  ]

type alias Bullet = {x : Int, y : Int, width : Int, height : Int, fill : String, friendly : Bool, dmg : Int, life : Int}
type BulletUpdater = BulletUpdater Bullet (Float -> BulletUpdater)

type alias Enemy = {x : Int, y : Int, r : Int, fill : String, life : Int}
type EnemyUpdater = EnemyUpdater Enemy (Float -> EnemyUpdater)


--- UPDATE ---


type Msg
  = Move Int Int 
  | Click 
  | MoveStars Time 
  | UpdateBullets Time
  | RemoveBullets Time
  | SpawnEnemy Time
  | UpdateEnemies Time
  | RemoveEnemies Time
  | CheckForEnemyBulletCollision Time

update: Msg -> Model -> (Model, Cmd a)
update msg model =
  case msg of
    Move x y ->
      ({ model | x = x, y = y } , Cmd.none)
    Click ->
     ({ model | bullets=model.bullets ++ [spawnLinearBullet model True] }, Cmd.none)
    MoveStars t ->
      ({ model | stars=(List.map moveStarsHelper model.stars)}, Cmd.none)
    UpdateBullets t ->
      ({ model | bullets=(List.map (doBulletUpdate t) model.bullets) }, Cmd.none)
    RemoveBullets t ->
      ({ model | bullets=(List.filter filterBullets model.bullets) }, Cmd.none)
    SpawnEnemy t ->
      ({ model | enemies=model.enemies ++ [spawnBasicEnemy] }, Cmd.none)
    UpdateEnemies t ->
      ({ model | enemies=(List.map (doEnemyUpdate t) model.enemies) }, Cmd.none)
    RemoveEnemies t ->
      ({ model | enemies=(List.filter filterEnemies model.enemies) }, Cmd.none)
    CheckForEnemyBulletCollision t ->
      checkForEnemyBulletCollision model

--- BULLET LOGIC ---
  -- linear gun
  -- growing blob
  -- laser dmg over time
  -- boomerang, lives through 3 hits


getBullet : BulletUpdater -> Bullet
getBullet bulletUpdater =
  case bulletUpdater of
    BulletUpdater bullet updateFunction ->
      bullet

doBulletUpdate delta bulletUpdater =
    case bulletUpdater of
      BulletUpdater bullet updateFunction ->
        updateFunction delta

filterBullets : BulletUpdater -> Bool
filterBullets bulletUpdater =
  let
    bullet = getBullet bulletUpdater
  in
    if bullet.y <= 0 || bullet.y >= 1400 || bullet.x <= 0 || bullet.x >= 1360 || bullet.life <= 0 then
      False
    else
      True

-- LINEAR

spawnLinearBullet : Model -> Bool -> BulletUpdater
spawnLinearBullet model bool =
  let
    newBullet = {x=model.x-2, y=model.y-model.r-3, width=4, height=3, fill="#00FFFF", friendly=bool, dmg=500, life=1}
  in
    BulletUpdater newBullet (linearYBulletUpdate newBullet)

linearYBulletUpdate : Bullet -> Float -> BulletUpdater
linearYBulletUpdate bullet delta =
  let
    updatedBullet = { bullet | y=bullet.y-5 }
  in
    BulletUpdater updatedBullet (linearYBulletUpdate updatedBullet)

-- BLOB

spawnBlobBullet : Model -> Bool -> BulletUpdater
spawnBlobBullet model bool =
  let
    newBullet = {x=model.x-1, y=model.y-model.r-3, width=2, height=3, fill="#FFAAFF", friendly=bool, dmg=1, life=1}
  in
    BulletUpdater newBullet (blobBulletUpdate newBullet newBullet.x newBullet.y)

blobBulletUpdate : Bullet -> Int -> Int -> Float -> BulletUpdater
blobBulletUpdate bullet initialX initialY delta =
  let
    newY = bullet.y-1
    deltaY = newY - initialY
    newX = initialX + round(50*(sin(toFloat(deltaY)/25)))
    updatedBiggerBullet = { bullet | x=newX, width=bullet.width+1, dmg=bullet.dmg+1, y=newY }
  in
    BulletUpdater updatedBiggerBullet (blobBulletUpdate updatedBiggerBullet initialX initialY)

-- BOOMERANG

spawnBoomerangBullet : Model -> Bool -> BulletUpdater
spawnBoomerangBullet model bool =
  let
    newBullet = {x=model.x+2, y=model.y-model.r-3, width=10, height=3, fill="#AAFFAA", friendly=bool, dmg=500, life=3}
  in
    BulletUpdater newBullet (boomerangForwardBulletUpdate newBullet newBullet.x newBullet.y (newBullet.y-475))

boomerangForwardBulletUpdate : Bullet -> Int -> Int -> Int -> Float -> BulletUpdater
boomerangForwardBulletUpdate bullet initialX initialY targetY delta =
  let
    newY = bullet.y-2
    deltaY = newY - initialY
    newX = initialX + round(250*(sin(toFloat(-deltaY)/150)))
    updatedBoomerangBullet = { bullet | x=newX, y=newY }
  in
    if bullet.y <= targetY then
      BulletUpdater updatedBoomerangBullet (boomerangBackBulletUpdate updatedBoomerangBullet initialX initialY targetY)
    else
      BulletUpdater updatedBoomerangBullet (boomerangForwardBulletUpdate updatedBoomerangBullet initialX initialY targetY)

boomerangBackBulletUpdate : Bullet -> Int -> Int -> Int -> Float -> BulletUpdater
boomerangBackBulletUpdate bullet initialX initialY targetY delta =
  let
    newY = bullet.y+2
    deltaY = newY - initialY
    newX = initialX + round(250*(sin(toFloat(deltaY)/150)))
    updatedBoomerangBullet = { bullet | x=newX, y=newY }
  in
    BulletUpdater updatedBoomerangBullet (boomerangBackBulletUpdate updatedBoomerangBullet initialX initialY targetY)

-- SHRAPNEL

spawnShrapnelBullet : Model -> Bool -> BulletUpdater
spawnShrapnelBullet model bool =
  let
    newBullet = {x=model.x-4, y=model.y-model.r-3, width=8, height=3, fill="#FF00FF", friendly=bool, dmg=1, life=1}
  in
    BulletUpdater newBullet (shrapnelBulletUpdate newBullet (newBullet.y-100))

shrapnelBulletUpdate : Bullet -> Int -> Float -> BulletUpdater
shrapnelBulletUpdate bullet targetY delta =
  let
    updatedBullet = { bullet | y=bullet.y-(3)}
  in
    if bullet.y <= targetY then
      BulletUpdater updatedBullet (doNothingUpdate bullet)
    else
      BulletUpdater updatedBullet (shrapnelBulletUpdate updatedBullet targetY)


doNothingUpdate bullet delta =
  BulletUpdater bullet (doNothingUpdate bullet)


--- ENEMY LOGIC ---


getEnemy : EnemyUpdater -> Enemy
getEnemy enemyUpdater =
  case enemyUpdater of
    EnemyUpdater enemy updateFunction ->
      enemy

doEnemyUpdate delta enemyUpdater =
    case enemyUpdater of
      EnemyUpdater enemy updateFunction ->
        updateFunction delta

filterEnemies : EnemyUpdater -> Bool
filterEnemies enemyUpdater =
  let
    enemy = getEnemy enemyUpdater
  in
    if enemy.y <= 0 || enemy.y >= 1400 || enemy.x <= 0 || enemy.x >= 1360 || enemy.life <= 0 then
      False
    else
      True

spawnBasicEnemy : EnemyUpdater
spawnBasicEnemy =
  let
    newEnemy = {x=5, y=5, r=30, fill="#FF0000", life=1000}
  in
    EnemyUpdater newEnemy (basicEnemyUpdate newEnemy)

basicEnemyUpdate : Enemy -> Float -> EnemyUpdater
basicEnemyUpdate enemy delta =
  let
    updatedEnemy = { enemy | x=enemy.x+2, y=enemy.y+1 }
  in
    EnemyUpdater updatedEnemy (basicEnemyUpdate updatedEnemy)


--- COLLISION LOGIC ---


checkForEnemyBulletCollision : Model -> (Model, Cmd a)
checkForEnemyBulletCollision model =
  --(model, Cmd.none)
  ({ model | bullets=List.map(checkBulletBorders (List.map getEnemy model.enemies)) model.bullets,
      enemies=List.map(checkEnemyBorders (List.map getBullet model.bullets)) model.enemies }, Cmd.none)

checkBulletBorders : List Enemy -> BulletUpdater -> BulletUpdater
checkBulletBorders enemies bulletUpdater =
  let
    bullet = getBullet bulletUpdater
    collidedTargets = List.filter (collisionCheck bullet) enemies
    newBullet = { bullet | life=bullet.life - (List.length collidedTargets) }
  in
    BulletUpdater newBullet (linearYBulletUpdate newBullet)

checkEnemyBorders : List Bullet -> EnemyUpdater -> EnemyUpdater
checkEnemyBorders bullets enemyUpdater =
  let
    enemy = getEnemy enemyUpdater
    collidedTargets = List.filter (collisionCheck2 enemy) bullets
    newEnemy = { enemy | life=(enemy.life - (List.foldr (+) 0 (List.map(\b -> b.dmg) collidedTargets))) } -- sum up all dmg from bullets
  in
    EnemyUpdater newEnemy (basicEnemyUpdate newEnemy)

collisionCheck : Bullet -> Enemy -> Bool
collisionCheck bullet enemy =
  shipCollideWithLeftSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithRightSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithTopSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithBottomSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height

collisionCheck2 : Enemy -> Bullet -> Bool
collisionCheck2 enemy bullet =
  shipCollideWithLeftSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithRightSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithTopSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithBottomSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height

shipCollideWithLeftSide : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
shipCollideWithLeftSide cx cy r leftX width topY height =
  leftX + width >= cx - r && leftX + width <= cx + r && topY + height <= cy + r && topY >= cy - r 

shipCollideWithRightSide : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
shipCollideWithRightSide cx cy r leftX width topY height =
  leftX <= cx + r && leftX >= cx - r && topY + height <= cy + r && topY >= cy - r 

shipCollideWithTopSide : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
shipCollideWithTopSide cx cy r leftX width topY height =
  topY + height >= cy - r && topY + height <= cy + r && leftX >= cx - r && leftX + width <= cx + r

shipCollideWithBottomSide : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
shipCollideWithBottomSide cx cy r leftX width topY height =
  topY <= cy + r && topY >= cy - r && leftX >= cx - r && leftX + width <= cx + r
      

--- BACKGROUND STAR LOGIC ---


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


-- SUBSCRIPTIONS ---


subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
   [ Mouse.moves (\{x, y} -> Move x y),
     Mouse.clicks (\{x, y} -> Click),
     Time.every (50*Time.millisecond) MoveStars,
     Time.every (10*Time.millisecond) UpdateBullets,
     Time.every Time.millisecond RemoveBullets,
     Time.every second SpawnEnemy,
     Time.every (10*Time.millisecond) UpdateEnemies,
     Time.every Time.millisecond RemoveEnemies,
     Time.every Time.millisecond CheckForEnemyBulletCollision]


-- VIEW ---


view : Model -> Html Msg
view model = 
  svg [ viewBox "0 0 100% 100%", width "100%", height "99.77%" ]
      (
        [rect [ x (toString model.borders.left), y (toString model.borders.top), width (toString model.borders.right),
          height (toString model.borders.bottom), fill model.borders.fill ] []]
      ++ List.map(\star -> (circle [cx (toString star.x), cy (toString star.y), r "2", fill "#FFFFFF"] [])) model.stars
      ++ List.map(\enemy -> (circle [cx (toString enemy.x), cy (toString enemy.y), r (toString enemy.r), fill enemy.fill] []))
          (List.map getEnemy model.enemies)
      ++ List.map(\bullet -> (rect [x (toString bullet.x), y (toString bullet.y), width (toString bullet.width), height (toString bullet.height),
          fill bullet.fill] [])) (List.map getBullet model.bullets)
      ++ [circle [ cx (toString model.x), cy (toString model.y), r (toString model.r), fill model.fill ] []]
      )
