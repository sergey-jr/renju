module Types where

import Graphics.Gloss
import Data.Maybe
import Data.Matrix

data Player = Black | White
                     deriving (Show, Eq)

data Diagonal = L | R

newtype MainMenu = MainMenu [Button]

newtype OptionsMenu = OptionsMenu [Button]

data ButtonData a = ButtonData
  { buttonDataId  :: ButtonId
  , buttonRegular :: a
  , buttonActive  :: a
  }

data Button = Button
  { buttonId          :: ButtonId
  , buttonImage       :: Picture
  , buttonImageActive :: Picture
  , buttonHitbox      :: Maybe Hitbox
  , buttonPosition    :: Maybe Point 
  , buttonAction      :: App -> IO App
  }

newtype ButtonId = ButtonId String
  deriving (Eq)

data App = App
  { appGame :: Game
  , appPauseGame :: GamePause
  , appGameOptions :: GameOptions
  , appMenuStatus :: Maybe MenuStatus
  , images :: [ButtonData Picture]
  , posMouse :: Point
  }

data MenuStatus = MenuOpenMain (Maybe ButtonId) | MenuOpenOptions
  deriving (Eq)

data GamePause
  = GamePaused
  | GameContinues
  deriving (Eq)

data Opponent
  = Human
  | Computer ComputerDifficulty

data ComputerDifficulty = Hard | Easy

data GameOptions = GameOptions
  { optionOpponent :: Opponent
  , optionTimeLimit :: TimeLimit
  }

data Game = Game
  { gameBoard         :: Board
  , gameCurrentPlayer :: Player
  , gameStatus        :: Maybe GameStatus
  , gameTimer         :: Timer
  , gameHistory       :: Maybe Game
  }

data Timer = Timer
  { timerBlack  :: Float
  , timerWhite  :: Float
  }

data GameStatus
  = Victory Player
  | Tie
  deriving (Eq)

data MouseEvent = Click | Move
               
type Cell = Maybe Player 

type Board = Matrix Cell

type PointI = (Int, Int)

data TimeLimit = Limit | Nolimit
  deriving (Eq)

type Hitbox = (Point, Point)

sizeCell :: Float
sizeCell = 40.0

magicY :: Float
magicY = offsetY + fromIntegral sizeField / 2 * sizeCell

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