module Game where

import Graphics
import Types
import Files
import Graphics.Gloss


gameMain :: IO ()
gameMain    
 = do
   rejnzu     <- loadBMP "images/rejnzu.bmp"  --0
   whiteWin    <- loadBMP "images/white_win.bmp" --1
   blackWin  <- loadBMP "images/black_win.bmp"   --2
   tie        <- loadBMP "images/tie.bmp"         --3
   play_game  <- loadBMP "images/play_game.bmp"   --4
   texture    <- loadBMP "images/texture.bmp"     --5
   timer_b    <- loadBMP "images/timer_black.bmp"           --6
   timer_w    <- loadBMP "images/timer_white.bmp"           --7
   back'       <- loadBMP "images/bmp/back.bmp"    --8
   button     <- loadBMP "images/bmp/button.bmp"  --9
   fon        <- loadBMP "images/bmp/fon.bmp"    --10
   hard       <- loadBMP "images/bmp/hard.bmp"   --11
   hardness   <- loadBMP "images/bmp/hardness.bmp"--12
   load       <- loadBMP "images/bmp/load.bmp"   --13
   menu'       <- loadBMP "images/bmp/menu.bmp"   --14
   mode'       <- loadBMP "images/bmp/mode.bmp"   --15
   ok         <- loadBMP "images/bmp/ok.bmp"     --16
   options    <- loadBMP "images/bmp/options.bmp"--17
   options_1  <- loadBMP "images/bmp/options_1.bmp"--18
   save       <- loadBMP "images/bmp/save.bmp"     --19
   time'       <- loadBMP "images/bmp/time.bmp"     --20
   time_2     <- loadBMP "images/bmp/time_2.bmp"   --21
   time_text  <- loadBMP "images/bmp/time_text.bmp"--22
   cancel     <- loadBMP "images/bmp/cancel.bmp"        --23
   easy       <- loadBMP "images/bmp/easy.bmp"     --24
   h_h        <- loadBMP "images/bmp/h.bmp"        --25
   h_c        <- loadBMP "images/bmp/h_c.bmp"      --26
   
             
   go (Game (matrixFiling sizeField) 
        Black None 
        [rejnzu,
        whiteWin,
        blackWin,
        tie,
        play_game,
        texture,
        timer_b,
        timer_w,
        back',
        button,
        fon,
        hard,
        hardness,
        load,
        menu',
        mode',
        ok,
        options,
        options_1,
        save,
        time',
        time_2,
        time_text,cancel, 
        easy,h_h,h_c] 
        Nothing (20,20) 
        Empty 
        (Nolimit,Easy,HumHum,False) (0,0))