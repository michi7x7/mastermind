module GameIO (strToPegs, showComp, showCols) where

import GameRules

import Data.List (lookup)

chars = [('1', Red), ('2', Green), ('3', Blue), ('4', Yellow), ('5', Purple), ('6', Orange),
                    ('R', Red), ('G', Green), ('B', Blue), ('Y', Yellow), ('P', Purple), ('O', Orange),
                    ('r', Red), ('g', Green), ('b', Blue), ('y', Yellow), ('p', Purple), ('o', Orange)]

-- convert user-input to data we can work with
charToPeg :: Char -> Maybe PegColor
charToPeg c = lookup c chars

strToPegs :: String -> PegCode
strToPegs = foldr con []
    where
        con c xs = case charToPeg c of
                    Just x -> x:xs
                    Nothing -> xs

showComp :: GuessRating -> String
showComp = map show'
    where
        show' PosColOK = '+'
        show' ColOK = 'o'


colors = [
    (Red,    "\ESC[0;31m[  Red   ]\ESC[0m"),
    (Green,  "\ESC[0;32m[ Green  ]\ESC[0m"),
    (Blue,   "\ESC[0;34m[  Blue  ]\ESC[0m"),
    (Yellow, "\ESC[0;33m[ Yellow ]\ESC[0m"),
    (Purple, "\ESC[0;35m[ Purple ]\ESC[0m"),
    (Orange, "\ESC[0;91m[ Orange ]\ESC[0m")]
    
showCol :: PegColor -> String
showCol c = case lookup c colors of
                Just s -> s
                Nothing -> "xxx"

showCols :: PegCode -> String
showCols = concatMap ((++" ") . showCol)
