import Html exposing (Html, text, div)
import Mouse exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import List
import Keyboard

{- INSTRUCTIONS
  - Use your mouse to move the plane
  - Left click to fire your weapon
  - 1-4 to cycle weapons

  GUNS
  Linear Shooter - each shot does 500 dmg
  Blob Shooter - each shot does more dmg with time
  Boomerang Shooter - each shot does 500 dmg when it moves forward, 1000 dmg when it moves backwards, can hit up to 3x
  Shranpel Shooter - each shot deals 1000 dmg, travels a certain distance, then explodes into 4 bullets that each do 500 dmg

  ENEMIES
  Basic Red Enemy - 1000 health

  QUESTIONS
  - Hide mouse
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
  , invincible : Bool
  , gunSelected : Int
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
    invincible=True, -- CHANGE TO TRUE TO CHEAT
    gunSelected=1,
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
type BulletUpdater = BulletUpdater Bullet (List BulletUpdater) (List EnemyUpdater -> BulletUpdater)

type alias Enemy = {x : Int, y : Int, r : Int, fill : String, life : Int}
type EnemyUpdater = EnemyUpdater Enemy (List BulletUpdater) (Model -> EnemyUpdater)


--- UPDATE ---


type Msg
  = Move Int Int 
  | Click
  | Key Int
  | MoveStars Time 
  | UpdateBullets Time
  | RemoveBullets Time
  | SpawnEnemy Time
  | UpdateEnemies Time
  | RemoveEnemies Time
  | DetectPlayerHit Time
  

update: Msg -> Model -> (Model, Cmd a)
update msg model =
  case msg of
    Move x y ->
      ({ model | x = x, y = y } , Cmd.none)
    Click ->
      case model.gunSelected of
        1 -> ({ model | bullets=model.bullets ++ [spawnLinearBullet model True] }, Cmd.none)
        2 -> ({ model | bullets=model.bullets ++ [spawnBlobBullet model True] }, Cmd.none)
        3 -> ({ model | bullets=model.bullets ++ [spawnBoomerangBullet model True] }, Cmd.none)
        4 -> ({ model | bullets=model.bullets ++ [spawnShrapnelBullet model True] }, Cmd.none)
        _ -> ({ model | bullets=model.bullets ++ [spawnLinearBullet model True] }, Cmd.none)
    Key num ->
      case num of
        1 -> ({ model | gunSelected=1}, Cmd.none)
        2 -> ({ model | gunSelected=2}, Cmd.none)
        3 -> ({ model | gunSelected=3}, Cmd.none)
        4 -> ({ model | gunSelected=4}, Cmd.none)
        _ -> (model, Cmd.none)
    MoveStars t ->
      ({ model | stars=(List.map moveStarsHelper model.stars)}, Cmd.none)
    UpdateBullets t ->
      let 
        updatedBulletsAndNewBullets = List.map(doBulletUpdate model.enemies) model.bullets
        updatedBulletUpdaters = List.map(\(b1,b2) -> b1) updatedBulletsAndNewBullets
        newBullets = List.map(\(b1,b2) -> b2) updatedBulletsAndNewBullets
        result = List.foldr (++) updatedBulletUpdaters newBullets
      in
        ({ model | bullets=result }, Cmd.none)
    RemoveBullets t ->
      ({ model | bullets=(List.filter filterBullets model.bullets) }, Cmd.none)
    SpawnEnemy t ->
      ({ model | enemies=model.enemies ++ [spawnSuicideEnemy] }, Cmd.none)
    UpdateEnemies t ->
      let 
        updatedEnemiesAndNewBullets = List.map(doEnemyUpdate model) model.enemies
        updatedEnemyUpdaters = List.map(\(e,b) -> e) updatedEnemiesAndNewBullets
        newBullets = List.map(\(e,b) -> b) updatedEnemiesAndNewBullets
        updatedBulletUpdaters = List.foldr (++) model.bullets newBullets
      in
        ({ model | enemies=updatedEnemyUpdaters, bullets=updatedBulletUpdaters}, Cmd.none)
    RemoveEnemies t ->
      ({ model | enemies=(List.filter filterEnemies model.enemies) }, Cmd.none)
    DetectPlayerHit t ->
      let
        hitRadius = 0
      in
        if model.invincible then
          (model, Cmd.none)
        else
          if playerHit model then
            ({ model | r = hitRadius }, Cmd.none)
          else
            (model, Cmd.none)

--- BULLET LOGIC ---
  -- linear gun
  -- growing blob
  -- boomerang, lives through 3 hits
  -- shrapnel gun that shoots more bullets


getBullet : BulletUpdater -> Bullet
getBullet bulletUpdater =
  case bulletUpdater of
    BulletUpdater bullet bulletUpdaters updateFunction ->
      bullet

doBulletUpdate enemies bulletUpdater =
    case bulletUpdater of
      BulletUpdater bullet newBulletUpdaters updateFunction ->
        (updateFunction enemies, newBulletUpdaters)

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
    BulletUpdater newBullet [] (linearYUpBulletUpdate newBullet)

linearYUpBulletUpdate : Bullet -> List EnemyUpdater -> BulletUpdater
linearYUpBulletUpdate bullet enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    updatedBullet = { bullet | y=bullet.y-5, life=bullet.life - (List.length collidedTargets) }
  in
    BulletUpdater updatedBullet [] (linearYUpBulletUpdate updatedBullet)

linearYDownBulletUpdate : Bullet -> List EnemyUpdater -> BulletUpdater
linearYDownBulletUpdate bullet enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    updatedBullet = { bullet | y=bullet.y+5, life=bullet.life - (List.length collidedTargets) }
  in
    BulletUpdater updatedBullet [] (linearYDownBulletUpdate updatedBullet)

linearXLeftBulletUpdate : Bullet -> List EnemyUpdater -> BulletUpdater
linearXLeftBulletUpdate bullet enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    updatedBullet = { bullet | x=bullet.x-5, life=bullet.life - (List.length collidedTargets) }
  in
    BulletUpdater updatedBullet [] (linearXLeftBulletUpdate updatedBullet)

linearXRightBulletUpdate : Bullet -> List EnemyUpdater -> BulletUpdater
linearXRightBulletUpdate bullet enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    updatedBullet = { bullet | x=bullet.x+5, life=bullet.life - (List.length collidedTargets) }
  in
    BulletUpdater updatedBullet [] (linearXRightBulletUpdate updatedBullet)

-- BLOB

spawnBlobBullet : Model -> Bool -> BulletUpdater
spawnBlobBullet model bool =
  let
    newBullet = {x=model.x-1, y=model.y-model.r-3, width=0, height=3, fill="#FFAAFF", friendly=bool, dmg=750, life=1}
  in
    BulletUpdater newBullet [] (blobBulletUpdate newBullet)

blobBulletUpdate : Bullet -> List EnemyUpdater -> BulletUpdater
blobBulletUpdate bullet enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    firstEnemy = Maybe.withDefault {x=-100, y=-100, r=30, fill="#FF0000", life=1000} (List.head enemies)
    newX = homeOnTarget bullet.x firstEnemy.x
    newY = homeOnTarget bullet.y firstEnemy.y
    maxWidth = 45
    maxSizeLinearBullet = { bullet | width=maxWidth, y=bullet.y-3,life=bullet.life - (List.length collidedTargets) }
    updatedBiggerLinearBullet = { bullet | width=bullet.width+1, dmg=bullet.dmg+10, y=bullet.y-3,
      life=bullet.life - (List.length collidedTargets) }
    maxSizeHomingBullet = { bullet | x=bullet.x+newX, width=maxWidth, y=bullet.y+newY, life=bullet.life - (List.length collidedTargets) }
    updatedBiggerHomingBullet = { bullet | x=bullet.x+newX, width=bullet.width+1, dmg=bullet.dmg+10, y=bullet.y+newY,
      life=bullet.life - (List.length collidedTargets) }
  in
    if List.length enemies == 0 && bullet.width >= maxWidth then
      BulletUpdater maxSizeLinearBullet [] (blobBulletUpdate maxSizeLinearBullet)
    else if List.length enemies == 0 then
      BulletUpdater updatedBiggerLinearBullet [] (blobBulletUpdate updatedBiggerLinearBullet)
    else if bullet.width >= maxWidth then
      BulletUpdater maxSizeHomingBullet [] (blobBulletUpdate maxSizeHomingBullet)
    else
      BulletUpdater updatedBiggerHomingBullet [] (blobBulletUpdate updatedBiggerHomingBullet)


-- BOOMERANG

spawnBoomerangBullet : Model -> Bool -> BulletUpdater
spawnBoomerangBullet model bool =
  let
    newBullet = {x=model.x+2, y=model.y-model.r-3, width=10, height=3, fill="#AAFFAA", friendly=bool, dmg=500, life=3}
  in
    BulletUpdater newBullet [] (boomerangForwardBulletUpdate newBullet newBullet.x newBullet.y (newBullet.y-575))

boomerangForwardBulletUpdate : Bullet -> Int -> Int -> Int -> List EnemyUpdater -> BulletUpdater
boomerangForwardBulletUpdate bullet initialX initialY targetY enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    newY = bullet.y-2
    deltaY = newY - initialY
    newX = initialX + round(250*(sin(toFloat(-deltaY)/150)))
    updatedBoomerangBullet = { bullet | x=newX, y=newY, life=bullet.life - (List.length collidedTargets) }
  in
    if bullet.y <= targetY then
      BulletUpdater updatedBoomerangBullet [] (boomerangBackBulletUpdate updatedBoomerangBullet initialX initialY targetY)
    else
      BulletUpdater updatedBoomerangBullet [] (boomerangForwardBulletUpdate updatedBoomerangBullet initialX initialY targetY)

boomerangBackBulletUpdate : Bullet -> Int -> Int -> Int -> List EnemyUpdater -> BulletUpdater
boomerangBackBulletUpdate bullet initialX initialY targetY enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies
    newY = bullet.y+2
    deltaY = newY - initialY
    newX = initialX + round(250*(sin(toFloat(deltaY)/250)))
    updatedBoomerangBullet = { bullet | x=newX, y=newY, life=bullet.life - (List.length collidedTargets),
      dmg=1000 }
  in
    BulletUpdater updatedBoomerangBullet [] (boomerangBackBulletUpdate updatedBoomerangBullet initialX initialY targetY)

-- SHRAPNEL

spawnShrapnelBullet : Model -> Bool -> BulletUpdater
spawnShrapnelBullet model bool =
  let
    newBullet = {x=model.x-2, y=model.y-model.r-3, width=8, height=8, fill="#FF00FF", friendly=bool, dmg=1000, life=1}
  in
    BulletUpdater newBullet [] (shrapnelBulletUpdate newBullet (newBullet.y-300))

shrapnelBulletUpdate : Bullet -> Int -> List EnemyUpdater -> BulletUpdater
shrapnelBulletUpdate bullet targetY enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    collidedTargets = List.filter (collisionCheck bullet) enemies

    updatedBullet = { bullet | y=bullet.y-3, life=bullet.life - (List.length collidedTargets)}
    updatedBulletSplit = { bullet | y=bullet.y-3, life=bullet.life - (List.length collidedTargets), dmg=500, fill="#7F007F"}
    newLeftRightBullet = {x=bullet.x, y=bullet.y, width=bullet.width, height=bullet.height, fill="#7F007F", friendly=bullet.friendly, dmg=500, life=1}
    newDownBullet = {x=bullet.x, y=bullet.y, width=bullet.width, height=bullet.height, fill="#7F007F", friendly=bullet.friendly, dmg=500, life=1}

    newLeftBulletUpdater = BulletUpdater newLeftRightBullet [] (linearXLeftBulletUpdate newLeftRightBullet)
    newRightBulletUpdater = BulletUpdater newLeftRightBullet [] (linearXRightBulletUpdate newLeftRightBullet)
    newDownBulletUpdater = BulletUpdater newDownBullet [] (linearYDownBulletUpdate newDownBullet)
  in
    if bullet.y <= targetY then
      BulletUpdater updatedBulletSplit [newLeftBulletUpdater, newRightBulletUpdater, newDownBulletUpdater] (linearYUpBulletUpdate updatedBulletSplit)
    else
      BulletUpdater updatedBullet [] (shrapnelBulletUpdate updatedBullet targetY)


--- ENEMY LOGIC ---


getEnemy : EnemyUpdater -> Enemy
getEnemy enemyUpdater =
  case enemyUpdater of
    EnemyUpdater enemy bulletUpdaters updateFunction ->
      enemy

doEnemyUpdate : Model -> EnemyUpdater -> (EnemyUpdater, List BulletUpdater)
doEnemyUpdate delta enemyUpdater =
    case enemyUpdater of
      EnemyUpdater enemy bulletUpdaters updateFunction ->
        (updateFunction delta, bulletUpdaters)

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
    newEnemy = {x=4, y=5, r=30, fill="#FF0000", life=1000}
  in
    EnemyUpdater newEnemy [] (basicEnemyUpdate newEnemy)

basicEnemyUpdate : Enemy -> Model -> EnemyUpdater
basicEnemyUpdate enemy model =
  let
    bullets = List.map getBullet model.bullets
    collidedTargets = List.filter (collisionCheck2 enemy) bullets
    updatedEnemy = { enemy | x=enemy.x+2, y=enemy.y+1,
      life=(enemy.life - (List.foldr (+) 0 (List.map(\b -> b.dmg) collidedTargets))) }
    newBullet = {x=enemy.x-2, y=enemy.y+enemy.r+3, width=4, height=3, fill="#FF0000", friendly=False, dmg=500, life=1}
    newBulletUpdater = BulletUpdater newBullet [] (linearYDownBulletUpdate newBullet)
  in
    if enemy.x % 100 == 0 then
      EnemyUpdater updatedEnemy [newBulletUpdater] (basicEnemyUpdate updatedEnemy)
    else
      EnemyUpdater updatedEnemy [] (basicEnemyUpdate updatedEnemy)

spawnSuicideEnemy :EnemyUpdater
spawnSuicideEnemy =
  let
    newEnemy = {x=4, y=5, r=30, fill="#7F0000", life=500}
  in
    EnemyUpdater newEnemy [] (suicideEnemyUpdate newEnemy)

suicideEnemyUpdate : Enemy -> Model -> EnemyUpdater
suicideEnemyUpdate enemy model =
  let
    bullets = List.map getBullet model.bullets
    collidedTargets = List.filter (collisionCheck2 enemy) bullets
    newX = homeOnTarget enemy.x model.x
    newY = homeOnTarget enemy.y model.y
    updatedEnemy = { enemy | x=enemy.x+newX, y=enemy.y+newY,
      life=(enemy.life - (List.foldr (+) 0 (List.map(\b -> b.dmg) collidedTargets))) }
  in
    EnemyUpdater updatedEnemy [] (suicideEnemyUpdate updatedEnemy)

-- HOMING LOGIC ---


homeOnTarget : Int -> Int -> Int
homeOnTarget yourPos targetPos =
  if yourPos < targetPos then
    2
  else if yourPos == targetPos then
    0
  else
    -2


--- COLLISION LOGIC ---


collisionCheck : Bullet -> Enemy -> Bool
collisionCheck bullet enemy =
  (shipCollideWithLeftSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithRightSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithTopSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithBottomSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height)
  && bullet.friendly

collisionCheck2 : Enemy -> Bullet -> Bool
collisionCheck2 enemy bullet =
  (shipCollideWithLeftSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithRightSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithTopSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithBottomSide enemy.x enemy.y enemy.r bullet.x bullet.width bullet.y bullet.height)
  && bullet.friendly

enemyBulletCollionCheck : Model -> Bullet -> Bool
enemyBulletCollionCheck model bullet =
  (shipCollideWithLeftSide model.x model.y model.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithRightSide model.x model.y model.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithTopSide model.x model.y model.r bullet.x bullet.width bullet.y bullet.height
  ||  shipCollideWithBottomSide model.x model.y model.r bullet.x bullet.width bullet.y bullet.height)
  && (not bullet.friendly)

enemyShipCollisionCheck : Model -> Enemy -> Bool
enemyShipCollisionCheck model enemy =
  (model.x + model.r >= enemy.x - enemy.r && model.x + model.r <= enemy.x + enemy.r
      && model.y + model.r <= enemy.y + enemy.r  && model.y - model.r >= enemy.y - enemy.r) ||
  (model.x - model.r <= enemy.x + enemy.r && model.x - model.r >= enemy.x - enemy.r
      && model.y + model.r <= enemy.y + enemy.r  && model.y - model.r >= enemy.y - enemy.r) ||
  (model.y + model.r >= enemy.y - enemy.r && model.y + model.r <= enemy.y + enemy.r
      && model.x - model.r >= enemy.x - enemy.r  && model.x + model.r <= enemy.x + enemy.r) ||
  (model.y - model.r <= enemy.y + enemy.r && model.y - model.r >= enemy.y - enemy.r
      && model.x - model.r >= enemy.x - enemy.r  && model.x + model.r <= enemy.x + enemy.r)

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

playerHit : Model -> Bool
playerHit model =
  (enemyBulletHitPlayer model model.bullets) || (enemyShipHitPlayer model model.enemies)

enemyBulletHitPlayer : Model -> List BulletUpdater -> Bool
enemyBulletHitPlayer model bulletUpdaters =
  let
    bullets = List.map getBullet bulletUpdaters
    hitBullets = List.filter(enemyBulletCollionCheck model) bullets
  in
    List.length hitBullets > 0

enemyShipHitPlayer : Model -> List EnemyUpdater -> Bool
enemyShipHitPlayer model enemyUpdaters =
  let
    enemies = List.map getEnemy enemyUpdaters
    hitEnemies = List.filter(enemyShipCollisionCheck model) enemies
  in
    List.length hitEnemies > 0


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


--- KEY PRESSES ---


-- 49-52 are keys 1-4
handleDown : Keyboard.KeyCode -> Msg
handleDown state =
    case state of
        49 -> Key 1
        50 -> Key 2
        51 -> Key 3
        52 -> Key 4
        _ -> Key 0


--- SUBSCRIPTIONS ---


subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
   [ Mouse.moves (\{x, y} -> Move x y),
     Mouse.clicks (\{x, y} -> Click),
     Keyboard.downs (\k -> handleDown k),
     Time.every (50*Time.millisecond) MoveStars,
     Time.every (10*Time.millisecond) UpdateBullets,
     Time.every Time.millisecond RemoveBullets,
     Time.every (2.5*second) SpawnEnemy,
     Time.every (12*Time.millisecond) UpdateEnemies,
     Time.every Time.millisecond RemoveEnemies,
     Time.every Time.millisecond DetectPlayerHit]

--- VIEW ---


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

