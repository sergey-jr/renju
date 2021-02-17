module Logic where

import Types
import Data.List
import Data.Maybe
import Data.Matrix

-- cancel of turn
getback :: Game -> Game
getback game | isJust (back game) = prevGame
             | otherwise = game
             where
                 Just prevGame = back game


-- game handler
checkGame :: PointI -> Game -> Game
checkGame (0,_) board = board
checkGame (_,0) board = board
checkGame coord game 
    | canPutAt coord curBoard =  Game
                            newBoard
                            (inversePlayer curPlayer)
                            winner'
                            (pic game)
                            (Just game)
                            (timer game)
                            (menu game)
                            (mode game)
                            (posMouse game)
    | otherwise             =  game
    where
        curBoard = field game
        curPlayer = player game
        newBoard = putIn coord curPlayer curBoard
        winner' = gameRules newBoard

-- get column by coordinate on screen
mainNumberCol :: Point -> Int
mainNumberCol x = numberCol
                  x
                  (offsetX - sizeCell - fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberCol :: Point -> Float -> Int
numberCol (x,_) n | x < n || x > (n + fromIntegral sizeField * sizeCell) = 0
                  | otherwise = div sizeField 2 + 1 + div (round (x - offsetX)) (round sizeCell)

-- get row by coordinate on screen
mainNumberRow :: Point -> Int
mainNumberRow x = numberRow
                          x
                         (offsetY + sizeCell + fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberRow :: Point -> Float -> Int
numberRow (_,y) n | y > n || y < (n - fromIntegral sizeField * sizeCell) = 0
                  | otherwise = div sizeField 2 - div (round ( y - offsetY)) (round sizeCell)

winner :: Eq a => Matrix (Maybe a) -> Maybe a
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    rows = toLists board
    columns = Data.List.transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = Data.List.transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . Data.List.transpose

-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq b => [Maybe b] -> [(Int, b)]
streaks [] = []
streaks (Nothing : xs) = streaks xs
streaks (element : xs) = (length ys, x) : streaks zs
  where
    (Just x) = element
    (ys, zs) = span (== Just x) xs

-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 4

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd

winFunc :: [Cell] -> Int -> Bool
winFunc _ 4 = True
winFunc (x : (y : xs)) ac | x == y && isJust x = winFunc (y : xs) (ac + 1) 
                          | otherwise = winFunc (y:xs) 0 
winFunc _ _ = False

-- Detrmine if game is end
-- None - nobody, Tie - board is full, otherwise (Victory Black |Victory White)

gameRules :: Board ->Win
gameRules board
            | isJust winner'
            = Victory player'
            | fullBoard board = Tie
            | otherwise = None
            where
                winner' = winner board
                Just player' = winner'

-- Is board full
fullRow :: [Cell] -> Bool
fullRow [] = True
fullRow (Nothing : _) = False
fullRow l = fullRow (tail l)

fullBoard :: Board -> Bool
fullBoard = fullRow . toList

-- change player
inversePlayer :: Player -> Player
inversePlayer Black = White
inversePlayer White = Black