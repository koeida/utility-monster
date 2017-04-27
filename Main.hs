module Main where

import Prelude hiding (Either(..))
import Data.Tuple
import Data.List
import Entities
import System.Console.ANSI
import System.IO
import GameMap
import TerminalDisplay


data Input = Up | Down | Left | Right | Exit deriving (Eq)

nth :: [a] -> Int -> Maybe a
nth [] 0 = Nothing
nth (x:_) 0 = Just x
nth (x:xs) n = nth xs (n - 1)

getCoord :: [[a]] -> Int -> Int -> Maybe a
getCoord l x y = case nth l y of 
    Just row -> nth row x
    Nothing -> Nothing

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

