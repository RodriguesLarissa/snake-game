import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import System.Random

data Direction = North | South | West | East

data Snake = Snake [(Float, Float)] Direction

newtype Fruit = Fruit (Float, Float)

data World = World { snake :: Snake, fruit :: Fruit, score :: Int }

render :: World -> Picture
render world
  | isGameOver world = pictures [renderBorders, renderSnake, gameOverText, gameOverMessage, renderScore world]
  | otherwise = pictures [renderBorders, renderSnake, renderFruit (worldFruit world), renderScore world]
  where
    renderSnake = pictures $ map renderSegment (snakeSegments (worldSnake world))
    gameOverText = translate (-200) 0 $ scale 0.5 0.5 $ color red $ textWithBorder "GAME OVER" red
    gameOverMessage = translate (-220) (-50) $ scale 0.25 0.25 $ color white $ textWithBorder "Pressione R para recomecar" white
    
renderScore :: World -> Picture
renderScore world = translate 200 230 $ scale 0.25 0.25 $ color white $ text ("Score: " ++ show (score world))

renderFruit :: Fruit -> Picture
renderFruit (Fruit (x, y)) = translate x y $ color red $ pictures [appleBody, appleStem]

appleRadius :: Float
appleRadius = segmentSize * 0.5

appleBody :: Picture
appleBody =
  pictures
    [ thickCircle (appleRadius * 0.4) (appleRadius * 0.8),
      translate (-appleRadius * 0.4) (appleRadius * 0.2) $ thickCircle (appleRadius * 0.35) (appleRadius * 0.4),
      translate (-appleRadius * 0.4) (-appleRadius * 0.4) $ thickCircle (appleRadius * 0.35) (appleRadius * 0.4)
    ]

appleStem :: Picture
appleStem = translate (-1) 10 $ color green $ rectangleSolid 2 6

textWithBorder :: String -> Color -> Picture
textWithBorder str color =
  pictures
    [ translate (-1) 1 $ coloredText color str,
      translate (-1) (-1) $ coloredText color str,
      translate 1 1 $ coloredText color str,
      translate 1 (-1) $ coloredText color str,
      coloredText color str
    ]

coloredText :: Color -> String -> Picture
coloredText c str = color c $ text str

renderSegment :: (Float, Float) -> Picture
renderSegment (x, y) = translate x y $ color snakeColor $ rectangleSolid segmentSize segmentSize

snakeSegments :: Snake -> [(Float, Float)]
snakeSegments (Snake segments _) = segments

worldFruit :: World -> Fruit
worldFruit (World _ fruitActual _) = fruitActual

segmentSize :: Float
segmentSize = 20

snakeColor :: Color
snakeColor = green

update :: Float -> World -> World
update dt world =
  if isGameOver world
    then world
    else updateSnake dt (moveSnake world)

isGameOver :: World -> Bool
isGameOver world = 
  let Snake segments _ = snake world
      (x, y) = head segments
   in x > 399 || x < -399 || y > 299 || y < -299 || hasCollisionWithBody (x, y) (tail segments)

moveSnake :: World -> World
moveSnake world =
  let Snake segments dir = snake world
      newHead = moveInDirection dir (head segments)
      newSegments = moveHead newHead segments
   in world {snake = Snake newSegments dir}

updateSnake :: Float -> World -> World
updateSnake _ world =
  if isGameOver world
    then world
    else unsafePerformIO (updateSnakeIO world)

updateSnakeIO :: World -> IO World
updateSnakeIO world = do
  let oldSnake = snake world
      newHead = moveInDirection (snakeDirection oldSnake) (head $ snakeSegments oldSnake)

  let collidedWithFruit = hasCollisionToFruit newHead [fruitPosition (worldFruit world)]

  newFruit <-
    if collidedWithFruit
      then generateRandomFruit
      else return (worldFruit world)

  let newSnake =
        if collidedWithFruit
          then Snake (snakeSegments oldSnake ++ [newHead]) (snakeDirection oldSnake)
          else Snake (moveHead newHead (snakeSegments oldSnake)) (snakeDirection oldSnake)

  let newScore =
        if collidedWithFruit
          then score world + 10
          else score world

  return world { snake = newSnake, fruit = newFruit, score = newScore }

worldSnake :: World -> Snake
worldSnake (World currentSnake _ _) = currentSnake

snakeDirection :: Snake -> Direction
snakeDirection (Snake _ direction) = direction

hasCollisionWithBody :: (Float, Float) -> [(Float, Float)] -> Bool
hasCollisionWithBody headPos segments = headPos `elem` segments

hasCollisionToFruit :: (Float, Float) -> [(Float, Float)] -> Bool
hasCollisionToFruit _ [] = False
hasCollisionToFruit headPos (segment : segments) =
  (distance headPos segment < segmentSize + apple) || hasCollisionToFruit headPos segments
  where
    apple = 8

moveInDirection :: Direction -> (Float, Float) -> (Float, Float)
moveInDirection North (x, y) = (x, y + segmentSize)
moveInDirection South (x, y) = (x, y - segmentSize)
moveInDirection West (x, y) = (x - segmentSize, y)
moveInDirection East (x, y) = (x + segmentSize, y)

moveHead :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
moveHead newHead segments = newHead : init segments

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) world = changeDirection North world
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) world = changeDirection South world
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) world = changeDirection West world
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) world = changeDirection East world
handleEvent (EventKey (Char 'r') Down _ _) _ = initialState  
handleEvent _ world = world

changeDirection :: Direction -> World -> World
changeDirection newDir world@(World currentSnake fruitPos scorePoints) =
  if isOppositeDirection currentDir newDir
    then world
    else World (Snake (snakeSegments currentSnake) newDir) fruitPos scorePoints
  where
    currentDir = snakeDirection currentSnake

isOppositeDirection :: Direction -> Direction -> Bool
isOppositeDirection North South = True
isOppositeDirection South North = True
isOppositeDirection West East = True
isOppositeDirection East West = True
isOppositeDirection _ _ = False

renderBorders :: Picture
renderBorders =
  pictures
    [ translate 0 300 $ color borderColor $ rectangleSolid 800 10,
      translate 0 (-300) $ color borderColor $ rectangleSolid 800 10,
      translate (-400) 0 $ color borderColor $ rectangleSolid 10 600,
      translate 400 0 $ color borderColor $ rectangleSolid 10 600
    ]

borderColor :: Color
borderColor = white

generateRandomPosition :: IO (Float, Float)
generateRandomPosition = do
  x <- randomRIO (-390, 390)
  y <- randomRIO (-290, 290)
  return (x, y)

generateRandomFruit :: IO Fruit
generateRandomFruit = do
  Fruit <$> generateRandomPosition

fruitPosition :: Fruit -> (Float, Float)
fruitPosition (Fruit pos) = pos

initialState :: World
initialState =
  let randomFruitPosition = unsafePerformIO generateRandomPosition
      initialSnake = Snake [(0, 0), (10, 0), (20, 0)] East
  in World initialSnake (Fruit randomFruitPosition) 0

main :: IO ()
main = do
  play exhibit bgColor fps initialState render handleEvent update
  where
    exhibit = InWindow "Snake Game" (800, 600) (100, 100)
    bgColor = black
    fps = 8
