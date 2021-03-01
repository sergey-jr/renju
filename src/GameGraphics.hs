module GameGraphics where

import Graphics.Gloss
import Data.Matrix
import Utils
import Types

-- | function that draw background of board, game name, and game winner (if exisits)
showGameStatus :: Game -> [ButtonData Picture] -> [Picture]
showGameStatus game pics
 = map drawButton [textureBtn, gameNameBtn, winnerBtn]
  where
    
    textureBtn = mkButton (ButtonId "Texture")
                  Nothing
                  (Just (offsetX, offsetY + 1))
                  return
                  pics
    gameNameBtn = mkButton (ButtonId "Game Name")
                  Nothing
                  (Just (offsetX, offsetY + 380))
                  return
                  pics
    winnerPlayer = gameEnd game
    winnerBtn = case winnerPlayer of
                  Just Tie -> mkButton (ButtonId "Tie")
                              Nothing
                              (Just (offsetX, offsetY + 330))
                              return
                              pics
                  Just (Victory White) -> mkButton (ButtonId "White win")
                            Nothing
                            (Just (offsetX, offsetY + 330))
                            return
                            pics
                  Just (Victory Black) ->  mkButton (ButtonId "Black win")
                                            Nothing
                                            (Just (offsetX, offsetY + 330))
                                            return
                                            pics 
                  Nothing -> mkButton (ButtonId "Playing")
                              Nothing
                              (Just (offsetX, offsetY + 330))
                              return
                              pics        
                  

-- | draw timer if exists
drawTimer :: App -> [ButtonData Picture] -> [Picture]
drawTimer app pics 
  | optionTimeLimit (appGameOptions app) == Nolimit = [Blank]
  | otherwise 
  = [timerBlackPic, timerWhitePic]
  where
    c1         = fromIntegral sizeField / 2 * sizeCell - 35
    c2         = - c1 - 50
    c3         = c1 + 20
    c4         = c2 + 30
    color' z | z< 10  = color red
            | otherwise  = color black
    Timer x y = gameTimer (appGame app)
    timerBlackBtn = mkButton (ButtonId "Timer Black")
                    Nothing
                    Nothing
                    return
                    pics
    timerBlackPic = translate (offsetX + c2) magicY (color' x $ Scale 0.3 0.3 $ Text $ show x)
                  <>
                  translate 
                  (offsetX + c4) 
                  (offsetY + fromIntegral sizeField / 2 * sizeCell + 50) (buttonRegular (buttonData timerBlackBtn))
    timerWhiteBtn = mkButton (ButtonId "Timer White")
                    Nothing
                    Nothing
                    return
                    pics
    timerWhitePic = translate (offsetX + c1) magicY (color' y $ Scale 0.3 0.3 $ Text $ show y)
                  <>
                  translate 
                  (offsetX + c3) 
                  (offsetY + fromIntegral sizeField / 2 * sizeCell + 50) (buttonRegular (buttonData timerWhiteBtn))               

-- | draw Board
mainDrawBoard :: Board -> [Picture]
mainDrawBoard board
  = drawBoard
      (offsetX - sizeCell * fromIntegral ((nrows board - 1) `div` 2))
      (offsetY + sizeCell * fromIntegral ((nrows board - 1) `div` 2))
      (nrows board - 1)
      board

drawBoard :: Float -> Float -> Int -> Board -> [Picture]
drawBoard posX posY 0 board = drawLastRow 
                              (posX - (sizeCell / 2)) 
                              (posY + (sizeCell / 2)) 
                              $ GameGraphics.getRow 1 board
drawBoard posX posY n board
  = drawRow posX posY (GameGraphics.getRow 1 board)
    ++
    drawBoard
    posX (posY - sizeCell) (n - 1)
    (submatrix 2 (nrows board) 1 (ncols board) board)   

-- | get row of matrix as List 
getRow :: Int -> Matrix Cell -> [Cell] 
getRow n board = [board ! (i,j) 
              | i <- [n]
              , j <- [1 .. (ncols board)]]
                          
-- | draw cell
drawCell :: Float -> Float -> Cell -> [Picture]
drawCell pos_x pos_y s 
  = case s of
    Nothing -> [reckWire] 
    (Just x) -> [reckWire
                ,Translate 
                (pos_x - (sizeCell / 2))
                (pos_y + (sizeCell / 2)) $
                Color (colorPlayer x) $ circleSolid (sizeCell / 2)]
    where
    reckWire =  translate 
                pos_x 
                pos_y $
                rectangleWire sizeCell sizeCell

-- | convert player to color
colorPlayer :: Player -> Color
colorPlayer Black = black
colorPlayer White   = white

drawRow :: Float -> Float -> [Cell] -> [Picture]
drawRow pos_x pos_y [Just x] 
  = [Translate 
    (pos_x - (sizeCell / 2)) 
    (pos_y + (sizeCell / 2)) $
    Color (colorPlayer x) $ circleSolid (sizeCell / 2)]
drawRow _     _     [Nothing]         =     [Blank]

drawRow pos_x pos_y l
  = drawCell pos_x pos_y (head l) ++ drawRow (pos_x + sizeCell) pos_y (tail l)

drawLastRow :: Float -> Float -> [Cell] -> [Picture]
drawLastRow _ _ [] = [Blank]
drawLastRow pos_x pos_y l = drawLastCell pos_x pos_y (head l) ++ drawLastRow (pos_x + sizeCell) pos_y (tail l)

drawLastCell :: Float -> Float -> Cell -> [Picture]
drawLastCell _ _ Nothing                  =       [Blank]
drawLastCell pos_x pos_y (Just x)         
  = [Translate 
      pos_x 
      pos_y $ 
      Color (colorPlayer x) $ circleSolid (sizeCell / 2)]
