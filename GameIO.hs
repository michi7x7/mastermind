module GameIO (strToPegs, printCols, printWrLength, printRating, printWin, getGuess) where

import GameRules

import Data.List (lookup, repeat)
import Data.Maybe (fromJust)
import Control.Monad (forM, replicateM_)
import Text.Printf (printf)
import qualified System.Console.ANSI as Con

chars = [('1', Red), ('2', Green), ('3', Blue), ('4', Yellow), ('5', Purple), ('6', Orange),
         ('R', Red), ('G', Green), ('B', Blue), ('Y', Yellow), ('P', Purple), ('O', Orange),
         ('r', Red), ('g', Green), ('b', Blue), ('y', Yellow), ('p', Purple), ('o', Orange)]


-- convert user-input to data we can work with --
charToPeg :: Char -> Maybe PegColor
charToPeg c = lookup c chars

strToPegs :: String -> PegCode
strToPegs = foldr con []
    where
        con c xs = case charToPeg c of
                    Just x -> x:xs
                    Nothing -> xs

colors = [
    (Red,    "[  Red   ]"),
    (Green,  "[ Green  ]"),
    (Blue,   "[  Blue  ]"),
    (Yellow, "[ Yellow ]"),
    (Purple, "[ Purple ]"),
    (Orange, "[ Orange ]")]

showCol :: PegColor -> String
showCol c = case lookup c colors of
                Just s -> s
                Nothing -> error "well..."

-- pretty output --

conColors = [
    (Red, (Con.Red, Con.Dull)),
    (Green, (Con.Green, Con.Dull)),
    (Blue, (Con.Blue, Con.Dull)),
    (Yellow, (Con.Yellow, Con.Dull)),
    (Purple, (Con.Magenta, Con.Dull)),
    (Orange, (Con.Red, Con.Vivid))]


-- | sets Foreground to Intensity and Color
setConCol int col = Con.setSGR $ (:[]) $ Con.SetColor Con.Foreground int col
resetConCol = Con.setSGR $ (:[]) $ Con.Reset

printCols :: PegCode -> IO ()
printCols code = do
    forM code (\c -> do
        let (col, int) = fromJust $ lookup c conColors
        setConCol int col
        putStr $ showCol c )
    resetConCol

printWrLength :: IO ()
printWrLength = do
    setConCol Con.Dull Con.Red
    putStrLn "The code you entered has the wrong length!"
    resetConCol

printRating :: Int -> PegCode -> CompRes -> IO()
printRating n guess (n1,n2) = do
    putStr ">> "
    printCols guess
    putStr " ["
    setConCol Con.Vivid Con.Green
    putStr $ take n1 $ repeat '+'
    resetConCol
    putStr $ take n2 $ repeat 'o'
    putStr $ take (n - n1 - n2) $ repeat ' '
    putStrLn "]"

printWin :: PegCode -> IO ()
printWin code = do
    putStrLn ""
    setConCol Con.Vivid Con.Green
    putStr "++ "
    printCols code
    setConCol Con.Vivid Con.Green
    putStrLn "  ++++"
    resetConCol

prettyCount :: Int -> String
prettyCount n
    | n < length ns = ns !! n
    | otherwise     = (show n) ++ "th"
    where
        ns = ["last", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth"]

getGuess :: Int -> IO String
getGuess n = do
    printf "\nThis is your %s try. Your guess please:\n" $ prettyCount n
    str <- getLine
    Con.cursorUpLine 2
    Con.clearFromCursorToScreenEnd
    return str
