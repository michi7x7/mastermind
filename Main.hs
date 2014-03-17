module Main where

import GameRules
import GameIO
import HelperIO

import Data.List (sort)
import System.Environment (getArgs)
import Control.Monad.Error

import Text.Printf (printf)

data IterRes = ResAbort | ResWrongInput | ResFound
    
check :: IterRes -> Bool -> Either IterRes IterRes
check res False = Left res
check res True  = Right res

mainLoop :: PegCode -> Int -> IO (Maybe Int)
mainLoop code n = do
    str <- getGuess n

    let len = length code
        guess = strToPegs str
        eq   = code == guess
        comp = compPegs code guess
    
    let res = do -- Monad: Either IterRes ()
          check ResAbort $ str /= "quit"
          check ResWrongInput $ length guess == length code
          check ResFound $ not eq
          return ()

    case res of
        Right () -> do
            printRating len guess comp
            mainLoop code (n+1)

        Left ResWrongInput -> do
            printWrLength
            mainLoop code n

        Left ResAbort -> return Nothing
        
        Left ResFound  -> do
            printWin guess
            return $ Just n


main :: IO ()
main = do
    argss <- getArgs
    let args = parseArgs argss
    
    if Help `elem` args then
        printRules
    else
        putStrLn "Use the '-h' argument for help"

    let len = foldl (\n x -> case x of
                        CodeLen n' -> n'
                        _ -> n ) 4 args
    
    code <- genRandomCode len
    printf "A code with length %d has been prepared, lets begin!\n\n" len

    res <- mainLoop code 1

    case res of
        Just n  -> printf "Congratiulations, you took %d tries\n" n
        Nothing -> putStrLn "Too hard, huh?"
    _ <- getLine
    return ()
