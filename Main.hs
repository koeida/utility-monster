module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Data.Tuple
import Data.List

type Coord = (Int, Int)

data World = World { wHero :: Coord, utility :: [Int], gameMap :: [[Tile]], entities :: [Entity]}

data Entity = Entity { ai :: (Entity -> World -> Entity), symbol :: Char, wEntity :: Coord, direction :: Coord }

data Input = Up | Down | Left | Right | Exit deriving (Eq)

data TileType = Wall | Floor | Nihil deriving (Eq)

data TileOptions = Blocking | Diggable | Door | Locked deriving Eq

data Tile = Tile TileType [TileOptions] deriving Eq


nth :: [a] -> Int -> Maybe a
nth [] 0 = Nothing
nth (x:_) 0 = Just x
nth (x:xs) n = nth xs (n - 1)

getCoord :: [[a]] -> Int -> Int -> Maybe a
getCoord l x y = case nth l y of 
    Just row -> nth row x
    Nothing -> Nothing

tiles = [(Tile Wall [Blocking],'#'),
         (Tile Floor [],'.'),
         (Tile Nihil [Blocking], '?')]

tileToChar :: Tile -> Char
tileToChar tt = case (find (\(x, c) -> x == tt) tiles) of
    Just (_,c) -> c
    Nothing -> '?'

charToTile :: Char -> Tile
charToTile ch = case (find (\(Tile t x, c) -> c == ch) tiles) of
    Just (x,_) -> x
    Nothing -> Tile Nihil [Blocking]

mapX = 0
mapY = 1

map1 = strToMap [
       "#######################",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#.....................#",
       "#######################"]

strToMap :: [String] -> [[Tile]] 
strToMap = map (map charToTile) 

drawMap :: Int -> Int -> [[Tile]] -> IO ()
drawMap _ _ [] = return ()
drawMap cx cy (x:xs) = do
    setCursorPosition cy cx
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid White ]
    putStrLn (map tileToChar x)
    drawMap cx (cy + 1) xs

drawUtility :: [Int] -> IO () 
drawUtility us = do
    setCursorPosition 0 0
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid White ]
    putStr "Utility: "
    setUtilColor allTimeNet
    putStr $ show allTimeNet ++ "AT "
    setUtilColor lastTurnNet
    putStr $ show lastTurnNet ++ "LAST "

    where allTimeNet = last us - head us 
          lastTurnNet = case lastTurns of
            [] -> 0
            x:[] -> x
            x:y:[] -> y - x
          lastTurns = take 2 (reverse us)
    
setUtilColor :: Int -> IO ()
setUtilColor x  
    | x < 0 = setSGR [ SetConsoleIntensity BoldIntensity
                     , SetColor Foreground Vivid Red ]
    | otherwise = setSGR [ SetConsoleIntensity BoldIntensity
                         , SetColor Foreground Vivid Green ]

main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Utility Monster"
    gameLoop $ World (1,1) [0] map1 [Entity randomAI 'R' (5,5) (1,0)]
    clearScreen
    setCursorPosition 0 0
    setSGR [ Reset ]

gameLoop world@(World hero utility m es) = do
    clearScreen
    let es' = map (\e@(Entity aiFunc _ _ _) -> aiFunc e world) es
    drawUtility utility
    drawMap mapX mapY m
    drawHero hero
    mapM_ drawEntity es'
    input <- getInput
    case input of 
        Exit -> return ()
        _ -> handleDir  (world {entities = es'}) input

drawEntity :: Entity -> IO ()
drawEntity (Entity _ s (ex,ey) _) = do
    setCursorPosition (ey + mapY) (ex + mapX)
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid White ]
    putChar s

drawHero (hx, hy) = do
    setCursorPosition (hy + mapY) (hx + mapX)
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue ]
    putStrLn "@"

getInput = do
    char <- getChar
    case char of
        'q' -> return Exit
        'w' -> return Up
        's' -> return Down
        'a' -> return Left
        'd' -> return Right
        _ -> getInput

walkable (Tile t as) = all (/= Blocking) as 

allowedMove :: [[Tile]] -> Coord -> Bool
allowedMove m (nx,ny) = case getCoord m nx ny of
    Just t -> walkable t
    Nothing -> False

turnRight :: Coord -> Coord
turnRight c = case c of
    (0,-1) -> (1,0)
    (0,1) -> (-1,0)
    (1,0) -> (0,1)
    (-1,0) -> (0,-1)

randomAI :: Entity -> World -> Entity
randomAI e@(Entity _ _ (ex,ey) direction@(dx, dy)) (World _ _ m _) = 
    if allowedMove m newCoord 
        then e { wEntity = newCoord }
        else e { direction = turnRight direction }
    where newCoord@(nx,ny) = (ex + dx, ey + dy)

handleDir w@(World (hx, hy) utility m es) input = 
    if allowed then gameLoop $ w { wHero = newCoord } else gameLoop w
    where newCoord@(nx,ny) = case input of
                        Up -> (hx, hy - 1)
                        Down -> (hx, hy + 1)
                        Left -> (hx - 1, hy)
                        Right -> (hx + 1, hy)
          allowed = case getCoord m nx ny of
              Just t -> walkable t
              Nothing -> False

