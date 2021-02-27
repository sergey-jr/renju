module Files where

import Types
import Control.Exception
import Data.Matrix
import Data.List
import Data.Bifunctor

-- handle exceptions
processError :: FilePath -> IOException ->  IO()
processError f e = do 
    let err = show (e :: IOException)
    print ("Warning: Couldn't open " ++ f ++ ": " ++ err)

-- load game from file
loadState :: App -> IO App
loadState app = do
         board <- loadBoard
         (player, timer) <- loadTimer
         let game = Game {gameBoard=board, 
                         gameCurrentPlayer=player, 
                         gameStatus=Nothing, 
                         gameTimer=timer,
                         gameHistory=Nothing}
         return app {appGame=game}
         

loadTimer :: IO (Player,Timer)
loadTimer = do
    content <- readFile "save/save_time.txt"
    let (player', timers) = recPlayer content
    let (x, y) = bimap read read timers
    let timer = Timer {timerBlack=x, timerWhite=y}
    return (player', timer)

loadBoard :: IO Board
loadBoard = do
    content <- readFile "save/save_board.txt"
    return (recMatrix  1 1 content $ matrixFiling sizeField)

show' :: Cell -> [Char]
show' Nothing = "0"
show' (Just Black) = "1"
show' (Just White) = "2"

showRow :: [Cell] -> [Char]
showRow = unwords . map show'

showRows :: [[Cell ]] -> [[Char ]]
showRows = map showRow

-- save board
saveBoard :: Board -> IO Board
saveBoard board = do
    -- print "started save board"
    writeFile "./save/save_board.txt" (intercalate "\n" (showRows (toLists board)))
    -- print "end save board"
    return board

-- save timer
saveTimer :: Timer -> Player -> IO Timer
saveTimer timer player'
    | player' == White = 
        do 
            -- print "started save timer"
            writeFile "./save/save_time.txt" ("W " ++ show (timerBlack timer) ++ " " ++ show (timerWhite timer) ++ "\n")
            -- print "end save timer"
            return timer
    | otherwise = 
        do 
            -- print "started save timer"
            writeFile "./save/save_time.txt" ("B " ++ show (timerBlack timer) ++ " " ++ show (timerWhite timer) ++ "\n")
            -- print "end save timer"
            return timer

-- save game
saveState :: App  -> IO App
saveState app = 
    do
        let game = appGame app
        _ <- saveBoard (gameBoard game)
        _ <- saveTimer (gameTimer game) (gameCurrentPlayer game)
        return app

-- load matrix from strings
recMatrix  :: Int -> Int -> String -> Board -> Board
recMatrix  _ _ [] m = m
recMatrix  i j (x:xs) m             | x == '\n' = recMatrix  (i + 1) 1 xs m
                                    | x == ' '  = recMatrix  i j xs m 
                                    | otherwise = recMatrix i (j + 1) xs (setElem (f x) (i,j) m)
                                    where
                                        f '0' = Nothing
                                        f '1' = Just Black
                                        f '2' = Just White

-- load timers and which turn
recPlayer  :: String -> (Player,(String,String))
recPlayer  (x:xs)   | x == 'B' = (Black, recTime  xs [] ("",""))
                   | x == 'W' = (White , recTime  xs [] ("",""))
                   
recTime  :: String -> String -> (String,String) -> (String,String)
recTime  [] _ s = s
recTime  (x:xs) buf (a,b)   | x == '\n' = (a,reverse buf)
                            | x == ' '  = recTime  xs [] (reverse buf,"")
                            | otherwise = recTime  xs (x:buf) (a,b)         

matrixFiling :: Int -> Board 
matrixFiling n = matrix n n $ const Nothing