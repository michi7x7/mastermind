module MasterAI (AIState, AIStateM, aiTurn, initAIState, masterAI, updateAI) where

import GameRules
import Control.Monad (replicateM) --for permutations
import Control.Monad.State (StateT, get, put, gets, modify)
import Control.Monad.Identity
import System.Random (StdGen, random, randomR, newStdGen)

type AIGuess  = (PegCode, CompRes)

data AIState = AIState {
    aiTurn     :: Int,
    aiGuesses  :: [AIGuess],
    codeLen    :: Int,
    aiPossible :: [PegCode],
    aiRndGen   :: StdGen
    }

type AIStateM = StateT AIState Identity
    
colors = [Red, Green, Blue, Yellow, Purple, Orange]

initAIState :: Int -> IO AIState
initAIState n = do
        gen <- newStdGen
        return $ AIState 0 [] n p gen
    where
        p = replicateM n colors

masterAI :: AIStateM PegCode
masterAI = do
    cl   <- gets codeLen
    turn <- gets aiTurn
    guess <- case turn of
                1 -> return $ take cl $ cycle [Red,Green]
                2 -> return $ take cl $ cycle [Blue,Yellow]
                3 -> return $ take cl $ cycle [Purple,Orange]
                n -> chooseBestGuess
    return guess

updateAI :: PegCode -> CompRes -> AIStateM ()
updateAI guess res = modify $ 
            \st@(AIState turn guesses _ _ _) -> st{
                aiTurn    = turn + 1,
                aiGuesses = (guess, res): guesses }

chooseBestGuess :: AIStateM PegCode
chooseBestGuess = do
    st@(AIState _ guess _ poss gen) <- get
    let p = [c | c <- poss, all (\(g,r) -> compPegs c g == r) guess]
        l = length p
        (c, gen') = randomR (0, l-1) gen
    put st {aiRndGen = gen'}
    return (p !! c)
