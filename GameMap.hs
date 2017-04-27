module GameMap where

data TileType = Wall | Floor | Nihil deriving (Eq)

data TileOptions = Blocking | Diggable | Door | Locked deriving Eq

data Tile = Tile TileType [TileOptions] deriving Eq
