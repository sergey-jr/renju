module Graphics where

import Files
import Types
import Logic
import Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.Matrix

             
-- running game
go :: Game -> IO ()
go world = playIO (InWindow "Game Rejnzu" (800,800) (0,0)) 
               white 
               1
               world 
               convert 
               Graphics.handle
               update

-- update timer (if exist)
update ::Float -> Game -> IO Game
update _ game | p = return game
              | (x == 0) || (y == 0) = return game
              | limit == Nolimit = return game -- game without limit does not need changes
              | x == 1 && limit == Limit = return game { win = Victory White} -- time is up for Black
              | y == 1 && limit == Limit = return game { win = Victory Black } -- time is up for White
              | limit == Limit = return game {timer = f (player game)}
              | otherwise = return game
                where
                  (x, y) = timer game
                  (limit,_,_,p) = mode game
                  f White     = (x, y-1)
                  f Black   = (x-1, y)

-- draw game state
convert :: Game -> IO Picture
convert (Game m _ game p _ t' menu' (ti,ha,mo,pa) pos) = return
                           (Pictures $
                           drawPic game p ++ 
                           time ti t' p    ++ 
                           mainDrawField m ++
                           drawMenu p menu' (ti,ha,mo,pa) )
                          --  ++ [axisGrid 800 800 pos]



-- draw main menu and options
drawMenu :: [Picture] -> Menu ->(Time, Hard, Mode, Pause)->[Picture]
drawMenu p (Main 0) _ = drawMain p                                                       -- main menu
drawMenu p (Main n) _ = zipWith
                        (\ dy i -> translate offsetX (offsetY + dy) $ p !! i) 
                        [110, c]
                        [10, 9]
                        ++ tail (drawMain p)
                      where
                      c = fromIntegral sizeField / 2 * sizeCell - 140 - 60 * fromIntegral(n - 1)
drawMenu pic' Opt (t',h,m,_) = let                                                        -- options menu, draw choosen option
                                one = case t' of
                                            Limit     -> c 35 (-120)
                                            Nolimit  -> c 130 (-120)
                                two = case h of
                                             Hard     -> c 35 (-200)
                                             Easy     -> c 130 (-200)
                                three = case m of
                                             HumHum  -> c 35 (-280)
                                             HumComp -> c 130 (-280)      
                              in drawOpt pic' ++ [one,two,three]    
                                where
                                c x y = translate (offsetX + x) (offsetY + fromIntegral sizeField / 2 * sizeCell + y ) $ pic' !! 16                           
drawMenu _ _ _ = [Blank]

-- draw main menu
drawMain :: [Picture] -> [Picture]
drawMain p =  zipWith
            (\ dy i -> pic' 0 dy i) 
            [- c+110, - 60, - 140, - 200, - 260]
            [10, 14, 13, 19, 17]
            ++ [pic' 170 (- 65) 23]
              where 
              pic' x y i = translate (offsetX + x) (offsetY + fromIntegral sizeField / 2 * sizeCell + y ) $ p !! i
              c         = fromIntegral sizeField / 2 * sizeCell
-- draw options
drawOpt :: [Picture] -> [Picture]
drawOpt   p =  
               zipWith (`translate` 0)
                                [0, 170,  0, -120,  35, 130, -100 ,35, 130, -110,  35, 130,-150]
                       (zipWith (\dy i -> translate offsetX (offsetY + fromIntegral sizeField / 2 * sizeCell + dy) $ p !! i)
                                [-c+110, -65, -60, -120, -120, -120, -200, -200, -200, -280, -280, -280, -330]
                                [10, 23, 18,  22,  20,  21,  12,  11,  24,  15,  25,  26,   8])
               where
               c = fromIntegral sizeField / 2 * sizeCell

-- draw timer
time :: Time -> PointI -> [Picture] -> [Picture]
time Limit (x,y) p = zipWith (\ z dx -> translate (offsetX + dx) (offsetY + fromIntegral sizeField / 2 * sizeCell) $ Scale 0.3 0.3 $ Text $ show z)
             [x,y] [c2,c1]
             ++
             zipWith (\ dx i -> translate (offsetX + dx) (offsetY + fromIntegral sizeField / 2 * sizeCell + 50) $ p !! i)
             [c4,c3] [6,7]
             where 
             c1         = fromIntegral sizeField / 2 * sizeCell - 35
             c2         = - c1 - 50
             c3         = c1 + 20
             c4         = c2 + 30
time _ _ _ = [Blank]

-- draw winner
drawPic :: Win -> [Picture] -> [Picture]
drawPic x p = case x of
              None ->         zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                      [1,330,380] 
                                      [5,4,0]
                                
              _     ->        zipWith(\dy i -> translate offsetX (offsetY + dy) $ p !! i)
                                     [1,380,330]
                                     [5,0,msg x]
              where
                  msg (Victory Black) = 2
                  msg (Victory White)   = 1
                  msg Tie     = 3 

-- get row of matrix as List 
getRow :: Int -> Matrix Cell -> [Cell] 
getRow n m = [m ! (i,j) | i <- [n]
                          , j <- [1 .. (ncols m)]]
                          

drawCell :: Float -> Float -> Cell -> [Picture]
drawCell pos_x pos_y s = case s of
                             Nothing -> [reckWire] 
                             (Just x) -> [reckWire
                                         ,Translate 
                                         (pos_x - (sizeCell / 2))
                                         (pos_y + (sizeCell / 2)) $
                                         Color (col x) $ circleSolid (sizeCell / 2)]
                         where
                         reckWire =  translate 
                                     pos_x 
                                     pos_y $
                                     rectangleWire sizeCell sizeCell
                         col Black = black
                         col White   = white

drawRow :: Float -> Float -> [Cell] -> [Picture]
drawRow pos_x pos_y [Just x] = [Translate 
                                (pos_x - (sizeCell / 2)) 
                                (pos_y + (sizeCell / 2)) $
                                Color (col x) $ circleSolid (sizeCell / 2)]
                                where 
                                  col Black = black
                                  col White   = white
drawRow _     _     [Nothing]         =     [Blank]

drawRow pos_x pos_y l                 =     drawCell pos_x pos_y (head l)
                                            ++ drawRow (pos_x + sizeCell) pos_y (tail l)

mainDrawField :: Board -> [Picture]
mainDrawField m  = drawField 
                    (offsetX - sizeCell * fromIntegral ((nrows m - 1) `div` 2)) 
                    (offsetY + sizeCell * fromIntegral ((nrows m - 1) `div` 2)) 
                    (nrows m - 1) 
                    m

drawField :: Float -> Float -> Int -> Board -> [Picture]
drawField pos_x pos_y 0 m = drawLastRow 
                              (pos_x - (sizeCell / 2)) 
                              (pos_y + (sizeCell / 2)) 
                              $ Graphics.getRow 1 m
drawField pos_x pos_y n m  = drawRow pos_x pos_y (Graphics.getRow 1 m)
                              ++
                              drawField
                              pos_x (pos_y - sizeCell) (n - 1)
                              (submatrix 2 (nrows m) 1 (ncols m) m)

drawLastRow :: Float -> Float -> [Cell] -> [Picture]
drawLastRow _ _ [] = [Blank]
drawLastRow pos_x pos_y l = drawLastCell pos_x pos_y (head l)
                              ++ drawLastRow (pos_x + sizeCell) pos_y (tail l)

drawLastCell :: Float -> Float -> Cell -> [Picture]
drawLastCell _ _ Nothing                  =       [Blank]
drawLastCell pos_x pos_y (Just x)         =       [Translate 
                                                pos_x 
                                                pos_y $ 
                                                Color (col x) $ circleSolid (sizeCell / 2)]
                                          where
                                          col Black = black
                                          col White = white

-- handle events
handle :: Event -> Game -> IO Game
handle (EventKey (Char 'm') Down _ _) game 
  | menu game == Empty= return game {menu = Main 0, mode = newMode True}
  | otherwise = return game {menu = Empty, mode = newMode False}
  where
    (t',h,m,_) = mode game
    newMode f = (t', h, m, f)
handle (EventMotion (x,y)) game 
  | menu game == Empty || menu game == Opt = return game {posMouse = (x, y)}
  | otherwise = handleMenu Move (x,y) (game {posMouse = (x, y)}) 
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) game 
  | menu game /= Empty && menu game /= Opt = handleMenu Click (x,y) (game {mode = newMode True, posMouse = (x, y)})
  | menu game == Opt = handleMenu Click (x,y) (game {mode = newMode True, posMouse = (x, y)})
    where
      (t',h,m,_) = mode game
      newMode f = (t', h, m, f)

handle _ game | p = return game
  where
    (_, _, _, p) = mode game
handle (EventKey (SpecialKey KeySpace) Down _ _) game =  return (getback game)
handle _ game| win game /= Tie && win game /= None  = return game
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) game = return (checkGame (mainNumberRow (x,y),mainNumberCol (x,y)) game)
handle _ game = return game

-- handle menu events
t :: Types.Point -> Float -> Float -> Float -> Float -> Bool
t (x,y) dx1 dx2 dy1 dy2 = (x >= (offsetX + dx1) && x <= (offsetX + dx2)) && (y >= dy1 && y <= dy2)

handleMenu :: MouseEvent -> Types.Point -> Game -> IO Game
handleMenu event (x,y) (Game a b c d e f (Main g) (x1,y1,z1,p) _) 
  | t (x,y) (-120) 120 110 150
    = case event of
      Move -> return (menu' 1)
      Click-> loadGame (menu' g)
  | t (x,y) (-120) 120 50 90
    = case event of
      Move -> return (menu' 2)
      Click-> saveGame (menu' g)
  | t (x,y) (-120) 120 (-10) 30
    = case event of
      Move -> return (menu' 3)
      Click-> return (Game a b c d e f Opt (x1,y1,z1,p) (x,y))
  |t (x,y)  155 190 190 225
    = case event of
      Click->  return (Game a b c d e f Empty (x1,y1,z1,False) (x,y))
      Move ->  return (menu' g )
  | otherwise = return (menu' g )
  where 
  menu' n = Game a b c d e f (Main n) (x1,y1,z1,p) (x,y)
                                                     
                                                     
handleMenu Click (x,y) (Game a b c d e f Opt (x1,y1,z1,p) _)| t (x,y) 155 190 190 225
                                                         = return (Game a b c d e f Empty (x1,y1,z1,False) (x,y))
                                                            | t (x,y) (-180) (-60) 145 160
                                                         = case x1 of
                                                                Limit    -> return (Game a b c d e f Opt (Nolimit,y1,z1,p) (x,y))
                                                                Nolimit -> return (Game a b c d e (20,20) Opt (Limit,y1,z1,p) (x,y))
                                                            | t (x,y) (-180) (-20) 65 85
                                                         = case y1 of
                                                                Hard -> return (Game a b c d e f Opt (x1,Easy,z1,p) (x,y))
                                                                Easy -> return (Game a b c d e f Opt (x1, Hard,z1,p) (x,y))                                                             
                                                            | t (x,y) (-180) (-80) (-15) 0
                                                         = case z1 of 
                                                                HumComp -> return (Game a b c d e f Opt (x1,y1,HumHum,p) (x,y))
                                                                HumHum  -> return (Game a b c d e f Opt (x1,y1,HumComp,p) (x,y))
                                                            | t (x,y) (-180) (-130) (-85) (-35)
                                                              = return (Game a b c d e f (Main 0) (x1,y1,z1,p) (x,y))
                                                            | otherwise = return (Game a b c d e f Opt (x1,y1,z1,p) (x,y))                                                             