module GameMain where

import Graphics.Gloss

import Graphics
import Types
import Files
import Logic

loadButton :: ButtonData FilePath -> IO (ButtonData Picture)
loadButton btnData = 
     do
          active <- loadBMP (buttonActive btnData)
          regular <- loadBMP (buttonRegular btnData)
          return ButtonData 
               {
                    buttonDataId=buttonDataId btnData, 
                    buttonRegular=active, 
                    buttonActive=regular
               }

loadButtons :: [ButtonData FilePath] -> IO [ButtonData Picture]
loadButtons = mapM loadButton

buttons :: [ButtonData FilePath]
buttons = [
          ButtonData 
          {
               buttonDataId=ButtonId "Game Name",
               buttonRegular="images/renju.bmp", 
               buttonActive="images/renju.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "White win", 
               buttonRegular="images/white_win.bmp",
               buttonActive="images/white_win.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Black win",
               buttonRegular="images/black_win.bmp", 
               buttonActive="images/black_win.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Tie", 
               buttonRegular="images/tie.bmp", 
               buttonActive="images/tie.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Playing", 
               buttonRegular="images/playing.bmp", 
               buttonActive="images/playing.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Texture", 
               buttonRegular="images/texture.bmp", 
               buttonActive="images/texture.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Timer Black", 
               buttonRegular="images/timer_black.bmp", 
               buttonActive="images/timer_black.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Timer White", 
               buttonRegular="images/timer_white.bmp", 
               buttonActive="images/timer_white.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Back", 
               buttonRegular="images/bmp/back.bmp", 
               buttonActive="images/bmp/back.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Background", 
               buttonRegular="images/bmp/background.bmp", 
               buttonActive="images/bmp/background.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Hard", 
               buttonRegular="images/bmp/hard.bmp", 
               buttonActive="images/bmp/hard.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Easy", 
               buttonRegular="images/bmp/easy.bmp", 
               buttonActive="images/bmp/easy.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Opponent PC", 
               buttonRegular="images/bmp/opponent_computer.bmp", 
               buttonActive="images/bmp/opponent_computer.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Opponent Human", 
               buttonRegular="images/bmp/opponent_human.bmp", 
               buttonActive="images/bmp/opponent_human.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Limited", 
               buttonRegular="images/bmp/time_limited.bmp", 
               buttonActive="images/bmp/time_limited.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Unlimited", 
               buttonRegular="images/bmp/time_unlimited.bmp", 
               buttonActive="images/bmp/time_unlimited.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Difficulty", 
               buttonRegular="images/bmp/difficulty.bmp", 
               buttonActive="images/bmp/difficulty.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Opponent", 
               buttonRegular="images/bmp/opponent.bmp",
               buttonActive="images/bmp/opponent.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Time Limit", 
               buttonRegular="images/bmp/time_text.bmp", 
               buttonActive="images/bmp/time_text.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Options Text", 
               buttonRegular="images/bmp/options_text.bmp", 
               buttonActive="images/bmp/options_text.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Menu Text", 
               buttonRegular="images/bmp/menu.bmp",
               buttonActive="images/bmp/menu.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Close", 
               buttonRegular="images/bmp/close.bmp", 
               buttonActive="images/bmp/close.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Choosen", 
               buttonRegular="images/bmp/choosen.bmp", 
               buttonActive="images/bmp/choosen.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Load File", 
               buttonRegular="images/bmp/load.bmp", 
               buttonActive="images/bmp/load_active.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Options", 
               buttonRegular="images/bmp/options.bmp", 
               buttonActive="images/bmp/options_active.bmp"
          },
          ButtonData 
          {
               buttonDataId=ButtonId "Save File", 
               buttonRegular="images/bmp/save.bmp", 
               buttonActive="images/bmp/save_active.bmp"}
     ]


gameMain :: IO ()
gameMain    
 = do   
   btns <- loadButtons buttons

   let game' = 
        Game 
        {
          gameBoard=matrixFiling sizeField, 
          gameCurrentPlayer=Black, 
          gameStatus=Nothing , 
          gameTimer=Timer {timerBlack=60, timerWhite=60}, 
          gameHistory=Nothing
        }
   let game = (checkGame (8,8) game') {gameHistory = Nothing }
   let options = GameOptions {optionOpponent=Human, optionTimeLimit=Nolimit}
   let app = 
        App 
        {
             appGame=game,
             appPauseGame=GameContinues, 
             appGameOptions=options, 
             appMenuStatus=Nothing, 
             images=btns, 
             posMouse=(0, 0)
        }         
   go app