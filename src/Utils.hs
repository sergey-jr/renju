module Utils where

import Graphics.Gloss

-- optional axis for debuging
axisGrid :: Int -> Int -> (Float, Float ) -> Picture
axisGrid width height mousePos = yTicks <> coords <> verticalLine
  where
    width' = fromIntegral width / 2
    height' = fromIntegral height / 2
    horizontalLine = line (map (\x-> (x, 0)) [-width'..width'])
    verticalLine = line (map (\y-> (0, y)) [-height'..width'])
    line' = color red (translate (-0.5) 0 (line [(0, 0), (0, 1)]))
    scale' n = scale n n
    showNumber n | n == 0 = blank
                 | otherwise  = color red (text (show n))
    scaledText = scale' (1/15)
    xTicks = Pictures (map (\x-> translate x 10 (scaledText (showNumber x))) [-width', -width'+10..width'])
    yTicks = Pictures (map (\y-> translate 10 y (line' <> scaledText (showNumber y))) [-height', -height'+10..height'])
    coords =  Pictures [translate (width'-300) (height'-180) (scaledText  (text ("x=" ++ show (fst mousePos)))), 
                        translate (width'-200) (height'-180) (scaledText (text ("y=" ++ show (snd mousePos))))]