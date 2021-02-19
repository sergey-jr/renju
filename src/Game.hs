module Game where

import Graphics
import Types
import Files
import Graphics.Gloss
import Logic


loadImages :: IO [Picture]
loadImages = 
     do 
          let files = ["images/renju.bmp", "images/white_win.bmp",
               "images/black_win.bmp", "images/tie.bmp", "images/play_game.bmp",
               "images/texture.bmp", "images/timer_black.bmp",
               "images/timer_white.bmp", "images/bmp/back.bmp",
               "images/bmp/button.bmp", "images/bmp/fon.bmp",
               "images/bmp/hard.bmp", "images/bmp/hardness.bmp",
               "images/bmp/load.bmp", "images/bmp/menu.bmp",
               "images/bmp/mode.bmp", "images/bmp/ok.bmp",
               "images/bmp/options.bmp", "images/bmp/options_1.bmp",
               "images/bmp/save.bmp", "images/bmp/time.bmp",
               "images/bmp/time_2.bmp", "images/bmp/time_text.bmp",
               "images/bmp/cancel.bmp", "images/bmp/easy.bmp", "images/bmp/h.bmp",
               "images/bmp/h_c.bmp"]
          mapM loadBMP files

gameMain :: IO ()
gameMain    
 = do   
   pics <- loadImages

   let game = Game (matrixFiling sizeField) 
        Black None 
        pics
        Nothing (60,60) 
        Empty 
        (Nolimit,Easy,HumHum,False) (0,0)          
   go (checkGame (8,8) game){back = Nothing}