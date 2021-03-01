module Types where

import Graphics.Gloss
import Data.Maybe
import Data.Matrix

-- | data structure for player - black or white
data Player = Black | White
  deriving (Show, Eq)

data Diagonal = L | R

-- | main menu with its buttons
newtype MainMenu = MainMenu [Button]

-- | options menu with its buttons
newtype OptionsMenu = OptionsMenu [Button]

-- | general structure for holding buttons/pictures
data ButtonData a = ButtonData
  { 
    buttonDataId  :: ButtonId -- ^ ID of button
    ,buttonRegular :: a  -- ^ regular image/path for button
    ,buttonActive  :: a -- ^ active image/path for button
  }

-- | button data structure
data Button = Button
  { buttonData        :: ButtonData Picture
  , buttonHitbox      :: Maybe Hitbox -- ^ area for click/hover. Equal Nothing in case it is just picture
  , buttonPosition    :: Maybe Point -- ^ position of picture (Absulute). Equal Nothing if position is relative (now only one case)
  , buttonAction      :: App -> IO App -- ^ action that happens by click
  }

-- | synonym type for string to hold button ID
newtype ButtonId = ButtonId String
  deriving (Eq)

-- | data structure for application state
data App = App
  { appGame :: Game -- ^ state of game
  , appPauseGame :: GamePause -- ^ state of in-game pause
  , appGameOptions :: GameOptions -- ^ game options state
  , appMenuStatus :: Maybe MenuStatus -- ^ menu status. Nothing if menu closed (default)
  , images :: [ButtonData Picture] -- ^ all pictures/buttons of application
  , posMouse :: Point -- ^ mouse position for debuging reasons
  }

-- | Menu status - main with hovered button (if any), or options
data MenuStatus = MenuOpenMain (Maybe ButtonId) | MenuOpenOptions
  deriving (Eq)

data GamePause
  = GamePaused
  | GameContinues
  deriving (Eq)

-- | Opponent human or computer with some difficulty
data Opponent
  = Human
  | Computer ComputerDifficulty

data ComputerDifficulty = Hard | Easy

data TimeLimit = Limit | Nolimit
  deriving (Eq)

data GameOptions = GameOptions
  { optionOpponent :: Opponent
  , optionTimeLimit :: TimeLimit
  }

-- | data structure for game
data Game = Game
  { gameBoard         :: Board -- ^ board of the game
  , gameCurrentPlayer :: Player -- ^ current player
  , gameEnd           :: Maybe GameEnd -- ^ game status : Tie, some of player win, Nothing
  , gameTimer         :: Timer -- ^ timer for both players for game with limited time
  , gameHistory       :: Maybe Game -- ^ previous game
  , gameFuture        :: Maybe Game -- ^ future game
  }

data Timer = Timer
  { timerBlack  :: Float
  , timerWhite  :: Float
  }

data GameEnd
  = Victory Player
  | Tie
  deriving (Eq, Show)

data MouseEvent = Click | Move
               
type Cell = Maybe Player 

type Board = Matrix Cell

type PointI = (Int, Int)

type Hitbox = (Point, Point)

sizeCell :: Float
sizeCell = 40.0

-- | magic shift by Y
magicY :: Float
magicY = offsetY + fromIntegral sizeField / 2 * sizeCell

-- | size of field (size of matrix + 1)
sizeField :: Int
sizeField = 16

-- | center of desk -- X
offsetX :: Float
offsetX = 0

-- | center of desk -- Y
offsetY :: Float
offsetY = -50

--helpers
-- | update cell
updateCell :: Player -> Cell -> Cell
updateCell _ (Just x) = Just x
updateCell player' Nothing  = Just player'

-- | Try update an element at a given position in a list by a given function.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs = result
  where
    (left, right) = splitAt n xs
    (x, arr) | not (null right) = (head right, tail right)
             | otherwise = (last left, init left)
    newElement = f x
    result | not (null right) = left ++ [newElement] ++ arr
           | otherwise = arr ++ [newElement]

canPutAt :: (Int, Int) -> Board -> Bool 
canPutAt (n, m) board = isNothing (getElem n m board)