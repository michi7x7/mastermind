module HelperIO where

prettyCount :: Int -> String
prettyCount n
    | n < length ns = ns !! n
    | otherwise = (show n) ++ "th"
    where
        ns = ["last", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth"]



printRules :: IO ()
printRules = do
    putStrLn "--- MASTERMIND ---"
    putStrLn ""
    putStrLn "Command Line Arguments:"
    putStrLn " -h --help : Prints this Help"
    putStrLn " -n N      : Sets the code-length to N"
    putStrLn ""
    putStrLn "RULES: Try guessing the code with as little tries as possible"
    putStrLn " The code consists of 4 pegs with one of 6 colors"
    putStrLn " Possible colors are: Red, Green, Blue, Yellow, Purple, Orange"
    putStrLn " Use their first letter to build your guess"
    putStrLn ""
    putStrLn " On each try the computer will respond with a pattern to show"
    putStrLn " you how many blocks are in the right position, or of the right"
    putStrLn " color but at the wrong position"
    putStrLn " '+'' represents the first, 'o' the latter case"
    putStrLn ""
    putStrLn "Type 'quit' to give up"
    putStrLn ""
    putStrLn "GOOD LUCK!"
    putStrLn ""

data CmdArg = Help | CodeLen Int
    deriving (Eq)

parseArgs :: [String] -> [CmdArg]
parseArgs (s:ss)
    | s == "-h"     = Help : parseArgs ss
    | s == "--help" = Help : parseArgs ss
    | s == "-n"     = CodeLen (read h) : parseArgs ss
    | otherwise     = parseArgs ss
    where
        h = case length ss of
            0 -> ""
            _ -> head ss
parseArgs _ = []