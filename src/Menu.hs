module Menu where

import Types
import Graphics.Gloss
import Files
import Data.List
import Data.Maybe
import Utils

-- | declare main menu and its buttons
mainMenu :: [ButtonData Picture] -> MainMenu
mainMenu buttonPictures = MainMenu [ background, buttonLoadFile, buttonSaveFile, buttonOptions, closeButton, mainMenuText]
  where
    mainMenuText = mkButton (ButtonId "Menu Text")
                   Nothing
                   (Just (0 + offsetX, -60 + magicY))
                   return
                   buttonPictures
    buttonLoadFile = mkButton (ButtonId "Load File") 
                    (Just ((-110, 110), (105, 150))) 
                    (Just (0 + offsetX, -140 + magicY))
                    loadState
                    buttonPictures
    buttonSaveFile = mkButton (ButtonId "Save File") 
                    (Just ((-110, 110), (45, 90))) 
                    (Just (0 + offsetX, -200 + magicY))
                    saveState
                    buttonPictures
    buttonOptions = mkButton (ButtonId "Options")
                    (Just ((-110, 110), (-15, 30)))
                    (Just (0 + offsetX, -260 + magicY))
                    optionsClick
                    buttonPictures
    background  = mkButton (ButtonId "Background")
                  Nothing
                  (Just (0 + offsetX, 110+magicY-(fromIntegral sizeField / 2 * sizeCell)))
                  return
                  buttonPictures
    closeButton = mkButton (ButtonId "Close")
                  (Just ((155, 190), (190, 225)))
                  (Just (170 + offsetX, -65 + magicY))
                  closeClick
                  buttonPictures                                   

-- | declare options menu and its buttons
optionsMenu :: [ButtonData Picture] -> OptionsMenu
optionsMenu buttonPictures = OptionsMenu 
  [background, optionsMenuText, timeLimitButton, difficultyButton, 
  opponentButton, closeButton, backButton, choosen,
  opponentPC, opponentHuman, difficultyHard, difficultyEasy, timeLimit, timeUnlimit]
  where
    -- differrent pictures and text
    optionsMenuText = mkButton (ButtonId "Options Text")
                    Nothing 
                    (Just (0 + offsetX, -60 + magicY))
                    return
                    buttonPictures
    background  = mkButton (ButtonId "Background")
                  Nothing
                  (Just (0 + offsetX, 110+magicY-(fromIntegral sizeField / 2 * sizeCell)))
                  return
                  buttonPictures
    choosen = mkButton (ButtonId "Choosen")
              Nothing
              Nothing
              return
              buttonPictures
    opponentPC = mkButton (ButtonId "Opponent PC")
                 Nothing
                 (Just (130 + offsetX, -280 + magicY))
                 return
                 buttonPictures
    opponentHuman = mkButton (ButtonId "Opponent Human")
                 Nothing
                 (Just (35 + offsetX, -280 + magicY))
                 return
                 buttonPictures
    difficultyHard = mkButton (ButtonId "Hard")
                     Nothing
                     (Just (35 + offsetX , -200 + magicY))
                     return
                     buttonPictures
    difficultyEasy = mkButton (ButtonId "Easy")
                     Nothing
                     (Just (130 + offsetX, -200 + magicY))
                     return
                     buttonPictures
    timeLimit = mkButton (ButtonId "Limited")
                Nothing
                (Just (35 + offsetX, -120 + magicY))
                return
                buttonPictures
    timeUnlimit = mkButton (ButtonId "Unlimited")
                Nothing
                (Just (130 + offsetX, -120 + magicY))
                return
                buttonPictures                      
    -- different buttons                                                                                          
    timeLimitButton = mkButton (ButtonId "Time Limit")
                      (Just ((-180, -60), (145, 160)))
                      (Just (-120 + offsetX, -120+magicY))
                      timeLimitClick
                      buttonPictures
    difficultyButton = mkButton (ButtonId "Difficulty")
                       (Just ((-180, -20), (65, 85)))
                       (Just (-100 + offsetX, -200 + magicY))
                       difficultyClick
                       buttonPictures
    opponentButton = mkButton (ButtonId "Opponent")
                     (Just ((-180, -80), (-15, 0)))
                     (Just (-110 + offsetX, -280 + magicY))
                     opponentClick
                     buttonPictures
    closeButton = mkButton (ButtonId "Close")
                  (Just ((155, 190), (190, 225)))
                  (Just (170 + offsetX, -65 + magicY))
                  closeClick
                  buttonPictures
    backButton = mkButton (ButtonId "Back")
                 (Just ((-180, -130), (-85, -35)))
                 (Just (-150 + offsetX , -330 + magicY))
                 backClick
                 buttonPictures

-- | render for main menu with highlighted button (if exists)
renderMainMenu :: Maybe ButtonId -> MainMenu -> Picture
renderMainMenu highlighted (MainMenu buttons)
  = pictures (map drawButton buttons' ++ highlightedPic)
  where
    buttons' = case highlighted of
                Nothing -> buttons
                Just btnId -> filter (\btn -> buttonDataId (buttonData btn) /= btnId) buttons
    highlightedBtn = case highlighted of
                      Nothing -> []
                      Just btnId -> filter (\btn -> buttonDataId (buttonData btn) == btnId) buttons
    highlightedPic = map drawActiveButton highlightedBtn

-- | put choosen picture (V) near option
putChoosenAt :: Point -> Point -> Button -> Button -> Picture
putChoosenAt (x, y) (x1, y1) choosen button = translate x1 y1 (buttonActive (buttonData button))
                                              <> translate (x + offsetX) (y + magicY) (buttonRegular (buttonData choosen))

-- | render for options menu
renderOptionsMenu :: GameOptions -> OptionsMenu -> Picture
renderOptionsMenu gameOptions (OptionsMenu buttons) = pictures (map renderButton buttons)
  where
    Just choosen = find (\btn -> buttonDataId (buttonData btn) == ButtonId "Choosen") buttons
    renderButton button 
      | buttonDataId (buttonData button) == ButtonId "Unlimited" 
        = case optionTimeLimit gameOptions of
          Limit -> translate posX posY (buttonActive (buttonData button))
          Nolimit -> putChoosenAt (130, -120) (posX, posY) choosen button
      | buttonDataId (buttonData button) == ButtonId "Limited" 
        = case optionTimeLimit gameOptions of
          Nolimit -> translate posX posY (buttonActive (buttonData button))
          Limit -> putChoosenAt (35, -120) (posX, posY) choosen button
      | buttonDataId (buttonData button) == ButtonId "Easy" 
        = case optionOpponent gameOptions of
          Human -> putChoosenAt (130, -200) (posX, posY) choosen button
          Computer difficulty -> case difficulty of
                                  Hard -> translate posX posY (buttonActive (buttonData button))
                                  Easy -> putChoosenAt (130, -200) (posX, posY) choosen button
      | buttonDataId (buttonData button) == ButtonId "Hard" 
        = case optionOpponent gameOptions of
          Human -> translate posX posY (buttonActive (buttonData button))
          Computer difficulty -> case difficulty of
                                  Hard -> putChoosenAt (35, -200) (posX, posY) choosen button
                                  Easy -> translate posX posY (buttonActive (buttonData button))
      | buttonDataId (buttonData button) == ButtonId "Opponent PC" 
        = case optionOpponent gameOptions of
          Human -> translate posX posY (buttonActive (buttonData button))
          Computer _ -> putChoosenAt (130, -280) (posX, posY) choosen button
      | buttonDataId (buttonData button) == ButtonId "Opponent Human" 
        = case optionOpponent gameOptions of
        Human -> putChoosenAt (35, -280) (posX, posY) choosen button
        Computer _ -> translate posX posY (buttonActive (buttonData button))
      | buttonDataId (buttonData button) /= ButtonId "Choosen" = translate posX posY (buttonActive (buttonData button))
      | otherwise = blank 
      where
        Just (posX, posY) = buttonPosition button                                                                                                                           

optionsClick :: App -> IO App
optionsClick app = return app {appMenuStatus = Just MenuOpenOptions}

backClick :: App -> IO App 
backClick app = return app {appMenuStatus = Just (MenuOpenMain Nothing)}

closeClick :: App -> IO App
closeClick app = return app {appMenuStatus = Nothing, appPauseGame = GameContinues}

timeLimitClick :: App -> IO App 
timeLimitClick app = return app {appGameOptions = newOptions}
  where
    timelimit = case optionTimeLimit (appGameOptions app) of
                      Limit -> Nolimit
                      Nolimit -> Limit
    newOptions = (appGameOptions app){optionTimeLimit = timelimit}

difficultyClick :: App -> IO App
difficultyClick app = return app {appGameOptions = newOptions}
  where
    opponent = optionOpponent (appGameOptions app)
    newDifficulty = case opponent of
                      Human -> Easy 
                      Computer difficulty -> case difficulty of
                                              Easy -> Hard
                                              Hard -> Easy
    newOpponent = case opponent of
                    Human -> opponent
                    Computer _ -> Computer newDifficulty
    newOptions = (appGameOptions app){optionOpponent = newOpponent}

opponentClick :: App -> IO App 
opponentClick app = return app {appGameOptions = newOptions}
  where
    newOpponent = case optionOpponent (appGameOptions app) of
                    Human -> Computer Easy
                    Computer _ -> Human
    newOptions = (appGameOptions app){optionOpponent = newOpponent}

-- | check if mouse in hitbox for highlighting
mousePointerInHitbox :: Point -> Maybe Hitbox -> Bool
mousePointerInHitbox (x, y) (Just ((x1, x2), (y1, y2))) 
  = (x >= (offsetX + x1) && x <= (offsetX + x2)) && (y >= y1 && y <= y2)
mousePointerInHitbox _ Nothing  = False

-- | highlight button
activateButton :: Point -> MainMenu -> Maybe ButtonId
activateButton mousePos (MainMenu buttons) = btnId
  where
    button = find (mousePointerInHitbox mousePos . buttonHitbox) buttons
    btnId = case button of
              Nothing -> Nothing
              Just btn -> Just (buttonDataId (buttonData btn))

-- | menu handler
handleMenu :: MouseEvent -> Point -> App -> IO App
handleMenu Move mousePos app 
  | isJust (appMenuStatus app) && appMenuStatus app /= Just MenuOpenOptions = 
    return app {appMenuStatus = Just newMenuStatus, posMouse = mousePos}
  | otherwise = return app
  where
    menu = mainMenu (images app)
    newMenuStatus = MenuOpenMain (activateButton mousePos menu)

handleMenu Click mousePos app 
  | appMenuStatus app /= Just MenuOpenOptions = 
    mainMenuAction
  | appMenuStatus app == Just MenuOpenOptions = 
    optionsMenuAction
  | otherwise  = return app{posMouse = mousePos}
    where
      MainMenu mainBtns = mainMenu (images app)
      OptionsMenu optionsBtns = optionsMenu (images app)
      buttonMain = find (mousePointerInHitbox mousePos . buttonHitbox) mainBtns
      buttonOptions = find (mousePointerInHitbox mousePos . buttonHitbox) optionsBtns
      mainMenuAction = case buttonMain of
                        Nothing -> return app{posMouse = mousePos}
                        Just btn -> buttonAction btn (app{posMouse = mousePos})
      optionsMenuAction = case buttonOptions of
                            Nothing -> return app{posMouse = mousePos}
                            Just btn -> buttonAction btn (app{posMouse = mousePos})                 

