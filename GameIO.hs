module GameIO (strToPegs, showComp, showCols, printCols, printWrLength) where

import GameRules

import Data.List (lookup)
import Data.Maybe (fromJust)
import Control.Monad (forM)
import qualified System.Console.ANSI as Con

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
    (Red,    "[  Red   ]"),
    (Green,  "[ Green  ]"),
    (Blue,   "[  Blue  ]"),
    (Yellow, "[ Yellow ]"),
    (Purple, "[ Purple ]"),
    (Orange, "[ Orange ]")]

conColors = [
	(Red, (Con.Red, Con.Dull)),
	(Green, (Con.Green, Con.Dull)),
	(Blue, (Con.Blue, Con.Dull)),
	(Yellow, (Con.Yellow, Con.Dull)),
	(Purple, (Con.Magenta, Con.Dull)),
	(Orange, (Con.Red, Con.Vivid))]

showCol :: PegColor -> String
showCol c = case lookup c colors of
                Just s -> s
                Nothing -> "xxx"

showCols :: PegCode -> String
showCols = concatMap ((++" ") . showCol)


printCols :: PegCode -> IO ()
printCols code = do
            forM code (\c -> do
              let (col, int) = fromJust $ lookup c conColors
              Con.setSGR $ (:[]) $ Con.SetColor Con.Foreground int col
              putStr $ showCol c )
            Con.setSGR $ (:[]) $ Con.Reset

printWrLength :: IO ()
printWrLength = do
            Con.setSGR $ (:[]) $ Con.SetColor Con.Foreground Con.Dull Con.Red
            putStrLn "The code you entered has the wrong length!"
            Con.setSGR $ (:[]) $ Con.Reset

