module Utils where

import Graphics.Gloss
import Types
import Data.List

loadButtonPictures :: ButtonData FilePath -> IO (ButtonData Picture)
loadButtonPictures (ButtonData btnId regular active) = do
  regularPicture <- loadBMP regular
  activePicture <- loadBMP active
  return (ButtonData btnId regularPicture activePicture)

drawButton :: Button -> Picture
drawButton btn = translate posX posY (buttonImage btn)
  where
    Just (posX, posY) = buttonPosition btn

drawActiveButton :: Button -> Picture
drawActiveButton btn = translate posX posY (buttonImageActive btn)
  where
    Just (posX, posY) = buttonPosition btn

mkButton :: ButtonId -> Maybe Hitbox -> Maybe Point -> (App -> IO App) -> [ButtonData Picture] -> Button
mkButton btnId hitbox pos action imgs = 
  Button
  {buttonId = btnId, 
  buttonHitbox = hitbox,
   buttonImage = buttonRegular image,
   buttonImageActive = buttonActive image,
   buttonPosition = pos,
   buttonAction = action}
  where
    (Just image) = find (\btn -> buttonDataId btn == btnId) imgs

-- optional axis for debuging
axisGrid :: Int -> Int -> (Float, Float ) -> Picture
axisGrid width height mousePos = yTicks <> xTicks <> coords <> verticalLine <> horizontalLine
  where
    width' = floor (fromIntegral width / 2)
    height' = floor (fromIntegral height / 2)
    horizontalLine = line (map (\x->(fromIntegral x, 0)) [-width'..width'])
    verticalLine = line (map (\y -> (0,fromIntegral y)) [-height'..height'])
    line' = color red (translate (-0.5) 0 (line [(0, 0), (0, 1)]))
    scale' n = scale n n
    showNumber n | n == 0 = blank
                 | otherwise  = color red (text (show n))
    scaledText = scale' (1/20)
    rotatedText = rotate (-90)
    xTicks = Pictures (map (\x-> translate (fromIntegral x) 20 
                      (rotatedText (scaledText (showNumber x)))) [-width', -width'+5..width'])
    yTicks = Pictures (map (\y-> translate 10 (fromIntegral y) (scaledText (showNumber y))) [-height', -height'+5..height'])
    coords =  Pictures [translate (fromIntegral width'-300) (fromIntegral height'-180) 
                        (scaledText  (text ("x=" ++ show (fst mousePos)))), 
                        translate (fromIntegral width'-200) (fromIntegral height'-180) 
                        (scaledText (text ("y=" ++ show (snd mousePos))))]