module Graphics where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe ( isJust, isNothing )
import Types
import Logic
import Menu
import GameGraphics
import Utils

             
-- | running game
go :: App -> IO ()
go app = playIO (InWindow "Renju game" (800,800) (0,0)) 
               white 
               1
               app 
               drawApp 
               Graphics.handle
               updateIO


updateIO :: Float -> App -> IO App 
updateIO dt app = return (update dt app)

-- | update timer (if exist)
update :: Float -> App -> App
update dt app | appPauseGame app == GamePaused = app
              | limit == Nolimit = app -- game without limit does not need changes
              | timerBlack curTimer == 0 && limit == Limit =  app { appGame = victory White} -- time is up for Black
              | timerWhite curTimer == 0 && limit == Limit =  app { appGame = victory Black } -- time is up for White
              | limit == Limit =  app {appGame = newGame}
              | otherwise =  app
                where
                  limit = optionTimeLimit (appGameOptions app)
                  victory player = (appGame app){gameEnd = Just (Victory player)}
                  curTimer = gameTimer (appGame app)
                  delta = fromIntegral (floor dt `mod` 120)
                  newTimer = case gameCurrentPlayer (appGame app) of
                                  White -> curTimer {timerWhite = timerWhite curTimer - delta}
                                  Black -> curTimer {timerBlack = timerBlack curTimer - delta}
                  newGame = (appGame app){gameTimer =newTimer}

-- |  draw app state with/without menu
drawApp :: App -> IO Picture
drawApp app 
  | isNothing (appMenuStatus app) = 
    return (
      Pictures $
      showGameStatus (appGame app) (images app) ++
      drawTimer app (images app) ++
      mainDrawBoard (gameBoard (appGame app))
      --  ++ [axisGrid 800 800 (posMouse app)]
    )
  | otherwise = 
    return (
      Pictures $
      showGameStatus (appGame app) (images app) ++
      drawTimer app (images app) ++
      mainDrawBoard (gameBoard (appGame app)) ++
      drawMenu app
      --  ++ [axisGrid 800 800 (posMouse app)]
    )    

-- | draw menu
drawMenu :: App  -> [Picture]
drawMenu app | appMenuStatus app == Just MenuOpenOptions = [renderOptionsMenu options optionsMenu']
             | otherwise = [renderMainMenu highlighted mainMenu']
             where
               options = appGameOptions app
               optionsMenu' = optionsMenu (images app)
               highlighted = case appMenuStatus app of
                              Nothing -> Nothing
                              Just (MenuOpenMain Nothing) -> Nothing
                              Just (MenuOpenMain (Just btnId)) -> Just btnId
                              (Just MenuOpenOptions) -> Nothing 
               mainMenu' = mainMenu (images app)

-- | handle Game events
handle :: Event -> App -> IO App
handle (EventKey (SpecialKey KeyHome) Down _ _) app 
  | isNothing (appMenuStatus app) = return app {appMenuStatus = Just (MenuOpenMain Nothing) , appPauseGame = GamePaused}
  | otherwise = return app {appMenuStatus = Nothing , appPauseGame = GameContinues}

handle (EventMotion (x,y)) app 
  | isNothing (appMenuStatus app) || appMenuStatus app == Just MenuOpenOptions = return app {posMouse = (x, y)}
  | otherwise = handleMenu Move (x,y) (app {posMouse = (x, y)}) 

handle (EventKey (MouseButton LeftButton) Down _ (x,y)) app 
  | isJust (appMenuStatus app)
    = handleMenu Click (x,y) (app {posMouse = (x, y)})

handle _ app | appPauseGame app == GamePaused = return app

handle (EventKey (SpecialKey KeyLeft) Down _ _) app =  return app {appGame = newGame}
  where
    newGame = goBackward (appGame app)
handle (EventKey (SpecialKey KeyRight) Down _ _) app =  return app {appGame = newGame}
  where
    newGame = goForeward (appGame app)
handle _ app | isJust (gameEnd game)  = return app
  where
    game = appGame app
handle (EventKey (MouseButton LeftButton) Down _ (x,y)) app 
  = return app {appGame = newGame, posMouse = (x, y)}
  where 
    newGame = checkGame (mainNumberRow (x,y),mainNumberCol (x,y)) (appGame app)
handle _ app = return app