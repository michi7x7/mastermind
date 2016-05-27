module Main where

import GameRules
import GameIO
import HelperIO
import MasterAI

import Data.List (sort)
import System.Environment (getArgs)
import Control.Monad.State (runState)
import Control.Monad.Error

import Text.Printf (printf)

data IterRes = ResAbort | ResWrongInput | ResFound

check :: IterRes -> Bool -> Either IterRes IterRes
check res False = Left res
check res True  = Right res

mainAILoop :: PegCode -> AIState -> CompRes -> IO ()
mainAILoop code st res = do
    let len = length code
        (g, st') = runState (masterAI res) st
        eq       = code == g
        comp     = compPegs code g
    
    if eq then do
        printWin g
        return ()
    else do
        printRating len g comp
        mainAILoop code st' comp

mainAI :: IO ()
mainAI = do
    let len = 4
    code <- genRandomCode len
    st   <- initAIState len
    
    printf "A code with length %d has been prepared, lets begin!\n\n" len
    mainAILoop code st (0,0)

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

    if showHelp args then
        printRules
    else
        putStrLn "Use the '-h' argument for help"

    let len = codeLen args

    code <- genRandomCode len
    printf "A code with length %d has been prepared, lets begin!\n\n" len

    res <- mainLoop code 1

    case res of
        Just n  -> printf "Congratiulations, you took %d tries\n" n
        Nothing -> putStrLn "Too hard, huh?"
    _ <- getLine
    return ()
