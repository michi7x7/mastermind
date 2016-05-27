module Main where

import GameRules
import GameIO
import HelperIO
import MasterAI

import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Control.Monad.State (StateT, gets, evalStateT)
import Control.Monad.Error
import Control.Monad.Morph (hoist, generalize)

import Text.Printf (printf)
import Text.Read (readMaybe)

data IterRes = ResAbort | ResWrongInput | ResFound

check :: IterRes -> Bool -> Either IterRes IterRes
check res False = Left res
check res True  = Right res

    
-- PLAYER CODE ---
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

mainPlayer :: Int -> IO ()
mainPlayer len = do
    code <- genRandomCode len
    printf "A code with length %d has been prepared, lets begin!\n\n" len

    res <- mainLoop code 1

    case res of
        Just n  -> printf "Congratiulations, you took %d tries\n" n
        Nothing -> putStrLn "Too hard, huh?"
    _ <- getLine
    return ()

 
-- AI CODE --
type AIStateTIO = StateT AIState IO

mainAILoop :: PegCode -> AIStateTIO Int
mainAILoop code = do
    guess <- aiCall masterAI
    
    let len = length code
        eq   = code == guess
        comp = compPegs code guess
    
    aiCall $ updateAI guess comp
    
    if eq then do
        liftIO $ printWin guess
        tries <- gets aiTurn
        return tries
    else do
        liftIO $  printRating len guess comp
        mainAILoop code
    where
        aiCall = hoist generalize

mainAI :: String -> IO ()
mainAI arg = do
    let len = fromMaybe 0 $ readMaybe arg
    
    code <- if len > 0 && len <= 10 then
                genRandomCode len
            else
                return $ strToPegs arg
    
    putStr "The Code to guess is:  "
    printCols code
    putStrLn ""
    putStrLn ""
    
    tries <- initAIState (length code) >>= evalStateT (mainAILoop code)
    printf "Code found in %d tries\n" tries

-- MAIN --
main :: IO ()
main = do
    argss <- getArgs
    let args = parseArgs argss

    if showHelp args then
        printRules
    else
        putStrLn "Use the '-h' argument for help"

    case aiMode args of
        Nothing -> mainPlayer $ codeLen args
        Just s  -> mainAI s
