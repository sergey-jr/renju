module Files where

import Types
import System.IO.Unsafe
import Control.Exception
import Data.Matrix
import Data.List
import System.IO

-- load game from file
loadGame :: Game -> Game
loadGame game = game {field = field', player= player', timer=(time1, time2), posMouse=(0,0)}
         where
         tuple = unsafePerformIO (loadFile `catch` default')
         field' = fst tuple
         player' = fst(snd tuple)
         time1 = read(fst(snd(snd tuple)))
         time2 = read(snd(snd(snd tuple)))
         
-- handle exception on reading
default' :: IOException -> IO (Board ,(Player,(String,String)))
default' _ = return (matrixFiling sizeField,(Black,("0","0")))

-- load from file state
loadFile :: IO (Board ,(Player,(String,String)))
loadFile = do
           file1 <- readFile "save/save_board.txt"
           file2 <- readFile "save/save_time.txt"
           return (recMatrix  1 1 file1 $ matrixFiling sizeField, recPlayer  file2)

show' :: Cell -> [Char]
show' Nothing = "0"
show' (Just Black) = "1"
show' (Just White) = "2"

showRow :: [Cell] -> [Char]
showRow row = intercalate " " (map show' row)

showRows :: [[Cell ]] -> [[Char ]]
showRows = map showRow

-- handle exceptions on write
processErrorOnWrite :: IOException -> IO()
processErrorOnWrite e = do 
    let err = show (e :: IOException)
    hPutStr
        stderr
        ("Warning: Couldn't open " ++ "save/save_board.txt" ++ ": " ++ err)

-- save board
writeToFileBoard :: Board -> IO ()
writeToFileBoard board = do
    let write = writeFile "save/save_board.txt" (intercalate "\n" (showRows (toLists board)))
    let a = unsafePerformIO (write `catch` processErrorOnWrite)
    return a

-- save timer
writeToFileTimer :: PointI -> Player -> IO ()
writeToFileTimer (x, y) player'| player' == White = writeFile "save/save_time.txt" ("W " ++ show y)
                               | otherwise = writeFile "save/save_time.txt" ("B " ++ show x)

-- save game
saveBoard :: Game -> Game
saveBoard game = game
    where
        a = writeToFileBoard (field game)
        b = writeToFileTimer (timer game) (player game)

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
recPlayer  (x:xs)   | x == 'B' = (Black, recTime  (tail xs) [] ("",""))
                   | x == 'W' = (White , recTime  (tail xs) [] ("",""))
                   
recTime  :: String -> String -> (String,String) -> (String,String)
recTime  [] _ s = s
recTime  (x:xs) buf (a,b)   | x == '\n' = (a,reverse buf)
                            | x == ' '  = recTime  xs [] (reverse buf,"")
                            | otherwise = recTime  xs (x:buf) (a,b)         

matrixFiling :: Int -> Board 
matrixFiling n = matrix n n $ \ _ -> Nothing