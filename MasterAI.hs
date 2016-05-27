module MasterAI (AIState, AIStateM, initAIState, masterAI) where

import GameRules
import Control.Monad (replicateM) --for permutations
import Control.Monad.State (State, get, put, evalState)
import System.Random (StdGen, random, randomR, newStdGen)

type AIGuess  = (PegCode, CompRes)

data AIState = AIState {
    aiTurn     :: Int,
    lastGuess  :: Maybe PegCode,
    aiGuesses  :: [AIGuess],
    codeLen    :: Int,
    aiPossible :: [PegCode],
    aiRndGen   :: StdGen
    }

type AIStateM = State AIState
    
colors = [Red, Green, Blue, Yellow, Purple, Orange]

initAIState :: Int -> IO AIState
initAIState n = do
        gen <- newStdGen
        return $ AIState 1 Nothing [] n p gen
    where
        p = replicateM n colors

masterAI :: CompRes -> AIStateM PegCode
masterAI res = do
    st <- get
    let aiGss = case lastGuess st of
                    Nothing -> []
                    Just guess -> (guess, res): aiGuesses st 
        cl = codeLen st

    put st {aiGuesses = aiGss}
    
    guess <- case aiTurn st of
                1 -> return $ take cl $ cycle [Red,Green]
                2 -> return $ take cl $ cycle [Blue,Yellow]
                3 -> return $ take cl $ cycle [Purple,Orange]
                n -> chooseBestGuess
                        
    put st { aiTurn = aiTurn st + 1,
             lastGuess = Just guess,
             aiGuesses = aiGss}
    return guess

chooseBestGuess :: AIStateM PegCode
chooseBestGuess = do
    st@(AIState _ _ guess _ poss gen) <- get
    let p = [c | c <- poss, all (\(g,r) -> compPegs c g == r) guess]
        l = length p
        (c, gen') = randomR (0, l-1) gen
    put st {aiRndGen = gen'}
    return (p !! c)
