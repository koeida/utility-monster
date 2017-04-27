module TerminalDisplay where
import Data.List
import System.Console.ANSI
import System.IO
import Entities
import GameMap

tiles = [(Tile Wall [Blocking],'#'),
         (Tile Floor [],'.'),
         (Tile Nihil [Blocking], '?')]

mapX = 0
mapY = 1

tileToChar :: Tile -> Char
tileToChar tt = case (find (\(x, c) -> x == tt) tiles) of
    Just (_,c) -> c
    Nothing -> '?'

charToTile :: Char -> Tile
charToTile ch = case (find (\(Tile t x, c) -> c == ch) tiles) of
    Just (x,_) -> x
    Nothing -> Tile Nihil [Blocking]

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
