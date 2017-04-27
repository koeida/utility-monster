module Entities where

import GameMap

type Coord = (Int, Int)

data World = World { wHero :: Coord, utility :: [Int], gameMap :: [[Tile]], entities :: [Entity]}

data Entity = Entity { ai :: (Entity -> World -> Entity), symbol :: Char, wEntity :: Coord, direction :: Coord }
