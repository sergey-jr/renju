module Types where

import Graphics.Gloss
import Data.Maybe
import Data.Matrix

data Player = Black | White
                     deriving (Show, Eq)
data Victory a = Victory a | Tie | None deriving (Show, Eq)

data Diagonal = L | R

data Hard = Easy | Hard  deriving (Show, Eq)
data Mode = HumComp | HumHum  deriving (Show, Eq)
data Time = Limit | Nolimit  deriving (Show, Eq)
type Pause = Bool

data MouseEvent = Click | Move

data Menu = Main {anum :: Int} | Opt | Empty deriving (Show, Eq)

type Win = Victory Player
               
type Cell = Maybe Player 

type Board = Matrix Cell
type PointI = (Int,Int)
type Point = (Float,Float)

data Game = Game
          { field:: Board                  -- game board
          , player:: Player                 -- which turn
          , win  :: Win                    -- end of game flag
          , pic  :: [Picture]              -- loaded pictures
          , back :: Maybe Game             -- cancel turn (save previous state)
          , timer:: PointI                 -- timer for both players 
          , menu :: Menu                   -- munu objects
          , mode :: (Time,Hard,Mode,Pause) -- game mode (Time limit, Dificulty, PC/Human, Pause)
          , posMouse :: Types.Point
          }

sizeCell :: Float
sizeCell = 40.0

-- size of field (size of matrix + 1)
sizeField :: Int
sizeField = 16

-- center of desk
offsetX :: Float
offsetX = 0

offsetY :: Float
offsetY = -50

--helpers

putIn :: PointI -> Player -> Board -> Board
putIn (a, b) player' = setElem (Just player') (a,b)

-- | update cell
updateCell :: Player -> Cell -> Cell
updateCell _ (Just x) = Just x
updateCell player' Nothing  = Just player'

getFirst :: [a] -> (a, [a])
getFirst (x:xs) = (x, xs)

-- | get last element in list:
getLast :: [a] -> (a, [a])
getLast xs = (x, reverse xs')
  where
  (x, xs') = getFirst (reverse xs)


-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs = result
  where
    (left, right) = splitAt n xs
    (x, arr) | not (null right) = getFirst right
             | otherwise = getLast left
    newElement = f x
    result | not (null right) = left ++ [newElement] ++ arr
           | otherwise = arr ++ [newElement]

canPutAtRow :: Int -> [Maybe a] -> Bool
canPutAtRow _ [] = False 
canPutAtRow 0 (x:_) = isNothing x
canPutAtRow n (_:xs) = canPutAtRow (n-1) xs

canPutAt :: (Int, Int) -> Board -> Bool 
canPutAt (n, m) board = isNothing (getElem n m board)