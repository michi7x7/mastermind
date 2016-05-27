module HelperIO where

import qualified System.Console.ANSI as Con

-- Game Rules
printRules :: IO ()
printRules = do
    putStrLn "--- MASTERMIND ---"
    putStrLn ""
    putStrLn "Command Line Arguments:"
    putStrLn " -h --help   : Prints this Help"
    putStrLn " -n N        : Sets the code-length to N"
    putStrLn " -a {CODE|N} : Starts the AI to solve the Code"
    putStrLn ""
    putStrLn "RULES: Try guessing the code with as little tries as possible"
    putStrLn " The code consists of 4 pegs with one of 6 colors"
    putStrLn " Possible colors are: Red, Green, Blue, Yellow, Purple, Orange"
    putStrLn " Use their first letter to build your guess"
    putStrLn ""
    putStrLn " On each try the computer will respond with a pattern to show"
    putStrLn " you how many blocks are in the right position, or of the right"
    putStrLn " color but at the wrong position"
    putStrLn " '+' represents the first, 'o' the latter case"
    putStrLn ""
    putStrLn "Type 'quit' to give up"
    putStrLn ""
    putStrLn "GOOD LUCK!"
    putStrLn ""


data StartOpts = StartOpts {
       showHelp :: Bool,
       codeLen  :: Int,
       aiMode   :: Maybe String
       }

parseArgs :: [String] -> StartOpts
parseArgs (s:ss)
    | s == "-h"     = cont {showHelp = True}
    | s == "--help" = cont {showHelp = True}
    | s == "-n"     = cont {codeLen = (read h)}
    | s == "--ai"   = cont {aiMode  = Just h}
    | s == "-a"     = cont {aiMode  = Just h}
    | otherwise     = cont
    where
        cont = parseArgs ss
        h = case length ss of
            0 -> ""
            _ -> head ss
parseArgs _ = StartOpts {showHelp = False, codeLen = 4, aiMode = Nothing}
