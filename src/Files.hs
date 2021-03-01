module Files where

import Control.Exception
import Data.Matrix
import Data.List
import Data.Bifunctor
import Utils
import Types

-- | handle exceptions
processError :: FilePath -> IOException ->  IO()
processError f e = do 
    let err = show (e :: IOException)
    print ("Warning: Couldn't open " ++ f ++ ": " ++ err)

-- | load game from file
loadState :: App -> IO App
loadState app = do
         board <- loadBoard
         (player, timer) <- loadTimer
         let game = Game {gameBoard=board, 
                         gameCurrentPlayer=player, 
                         gameEnd=Nothing, 
                         gameTimer=timer,
                         gameHistory=Nothing,
                         gameFuture=Nothing}
         return app {appGame=game}
         
-- | load Timer
loadTimer :: IO (Player,Timer)
loadTimer = do
    content <- readFile "save/save_time.txt"
    let (player', timers) = readPlayerAndTimer content
    let (x, y) = bimap read read timers
    let timer = Timer {timerBlack=x, timerWhite=y}
    return (player', timer)

-- | load border
loadBoard :: IO Board
loadBoard = do
    content <- readFile "save/save_board.txt"
    return (readMatrix  1 1 content $ matrixFilling sizeField)

-- | converting cell to string
show' :: Cell -> String
show' Nothing = "0"
show' (Just Black) = "1"
show' (Just White) = "2"

showRow :: [Cell] -> String
showRow = unwords . map show'

showRows :: [[Cell ]] -> [String]
showRows = map showRow

-- | save board
saveBoard :: Board -> IO Board
saveBoard board = do
    writeFile "./save/save_board.txt" (intercalate "\n" (showRows (toLists board)))
    return board

-- | save timer
saveTimer :: Timer -> Player -> IO Timer
saveTimer timer player'
    | player' == White = 
        do 
            writeFile "./save/save_time.txt" ("W " ++ show (timerBlack timer) ++ " " ++ show (timerWhite timer) ++ "\n")
            return timer
    | otherwise = 
        do 
            writeFile "./save/save_time.txt" ("B " ++ show (timerBlack timer) ++ " " ++ show (timerWhite timer) ++ "\n")
            return timer

-- | save game
saveState :: App  -> IO App
saveState app = 
    do
        let game = appGame app
        _ <- saveBoard (gameBoard game)
        _ <- saveTimer (gameTimer game) (gameCurrentPlayer game)
        return app

-- | load matrix from strings
readMatrix  :: Int -> Int -> String -> Board -> Board
readMatrix  _ _ [] board = board
readMatrix  i j (x:xs) board             
    | x == '\n' = readMatrix  (i + 1) 1 xs board
    | x == ' '  = readMatrix  i j xs board 
    | otherwise = readMatrix i (j + 1) xs (setElem (f x) (i,j) board)
    where
        f '0' = Nothing
        f '1' = Just Black
        f '2' = Just White

-- | load timers and which turn
readPlayerAndTimer  :: String -> (Player,(String,String))
readPlayerAndTimer  (x:xs)   | x == 'B' = (Black, readTimer  xs [] ("",""))
                   | x == 'W' = (White , readTimer  xs [] ("",""))
                   
readTimer  :: String -> String -> (String,String) -> (String,String)
readTimer  [] _ s = s
readTimer  (x:xs) buf (a,b)   | x == '\n' = (a,reverse buf)
                            | x == ' '  = readTimer  xs [] (reverse buf,"")
                            | otherwise = readTimer  xs (x:buf) (a,b)         