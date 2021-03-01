module Logic where

import Types
import Data.List
import Data.Maybe
import Data.Matrix
import Graphics.Gloss

-- | go back -- a.k. cancel turn
goBackward :: Game -> Game
goBackward game | isJust (gameHistory game) = prevGame {gameFuture=Just game}
             | otherwise = game
             where
                 Just prevGame = gameHistory game

-- | go forward -- a.k. cancel goBackward
goForeward :: Game -> Game
goForeward game | isJust (gameFuture game) = futureGame
                | otherwise = game
                where
                    Just futureGame = gameFuture game

-- | game handler
checkGame :: PointI -> Game -> Game
checkGame (0,_) game = game
checkGame (_,0) game = game
checkGame coord game 
    | canPutAt coord curBoard && isNothing (winner curBoard) 
      =  game 
        {
          gameBoard=newBoard,
          gameCurrentPlayer=inversePlayer curPlayer,
          gameEnd=newStatus,
          gameHistory=Just game
        }
    | otherwise =  game
    where
        curBoard = gameBoard game
        curPlayer = gameCurrentPlayer game
        newBoard = putIn coord curPlayer curBoard
        newStatus = gameRules newBoard

-- | get column by coordinate on screen
mainNumberCol :: Point -> Int
mainNumberCol x = numberCol
                  x
                  (offsetX - sizeCell - fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberCol :: Point -> Float -> Int
numberCol (x,_) n | x < n || x > (n + fromIntegral sizeField * sizeCell) = 0
                  | otherwise = div sizeField 2 + 1 + div (round (x - offsetX)) (round sizeCell)

-- | get row by coordinate on screen
mainNumberRow :: Point -> Int
mainNumberRow x = numberRow
                          x
                         (offsetY + sizeCell + fromIntegral((sizeField - 1) `div` 2) * sizeCell) 
                         

numberRow :: Point -> Float -> Int
numberRow (_,y) n | y > n || y < (n - fromIntegral sizeField * sizeCell) = 0
                  | otherwise = div sizeField 2 - div (round ( y - offsetY)) (round sizeCell)

-- | determin winner
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

-- | set on board palyear on coordinates
putIn :: PointI -> Player -> Board -> Board
putIn (a, b) player' board = case winner board of
                              Just _ -> board
                              Nothing -> setElem (Just player') (a,b) board


-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq b => [Maybe b] -> [(Int, b)]
streaks [] = []
streaks (Nothing : xs) = streaks xs
streaks (Just x : xs) = (length ys, x) : streaks zs
  where
    (ys, zs) = span (== Just x) xs

-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 4

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd

-- | Detrmine if game is end
-- Nothing - game contiues, Tie - board is full, otherwise (Victory Black |Victory White)
gameRules :: Board -> Maybe GameEnd
gameRules board
            | isJust winner'
              = Just (Victory player)
            | fullBoard board = Just Tie
            | otherwise = Nothing
            where
                winner' = winner board
                Just player = winner'

-- | Is board full
fullRow :: [Cell] -> Bool
fullRow [] = True
fullRow (Nothing : _) = False
fullRow l = fullRow (tail l)

fullBoard :: Board -> Bool
fullBoard = fullRow . toList

-- | change player
inversePlayer :: Player -> Player
inversePlayer Black = White
inversePlayer White = Black