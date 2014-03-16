module GameRules (PegColor (..), PegCode, GuessOK (..), GuessRating, compPegs, genRandomCode) where

import Data.List (sort, elem, delete, (\\))
import System.Random

data PegColor = Red | Green | Blue | Yellow | Purple | Orange
    deriving (Show, Eq, Ord, Bounded, Enum)

data GuessOK = PosColOK | ColOK
    deriving (Show, Eq, Ord)

type PegCode  = [PegColor]
type GuessRating = [GuessOK]

compPegs :: PegCode -> PegCode -> GuessRating
compPegs code guess = let
        (n1, code', guess') = fnd1 code guess (0, [], [])
        n2 = (length guess') - (length $ guess' \\ code')
    in
       (take n1 $ repeat PosColOK) ++ (take n2 $ repeat ColOK)
    where
        -- takes all PosColOK-matches and returns the count
        fnd1 :: PegCode -> PegCode -> (Int, PegCode, PegCode) -> (Int, PegCode, PegCode)
        fnd1 (c:cs) (g:gs) (n1, c1, g1)
            | c == g    = fnd1 cs gs (n1 + 1, c1, g1)
            | otherwise = fnd1 cs gs (n1, c:c1, g:g1)
        fnd1 _ _ x = x

instance Random PegColor where
    random g =
        let
            min = fromEnum (minBound :: PegColor)
            max = fromEnum (maxBound :: PegColor)
        in case randomR (min, max) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g =
        case randomR (fromEnum a, fromEnum b) g of
                 (r, g') -> (toEnum r, g')

genRandomCode :: Int -> IO PegCode
genRandomCode n = do
    gen <- getStdGen
    return $ take n $ randoms gen