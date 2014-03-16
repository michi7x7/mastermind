mainIter :: PegCode -> IO IterRes
mainIter code = do
    liftIO $ putStrLn "Your guess please:"
    str <- liftIO $ getLine
    -- liftIO $ putStrLn ""

    if str == "quit" then
        return ResAbort
    else do
        let guess = strToPegs str
            eq   = code == guess
            comp = compPegs code guess

        case length guess == length code of
            False -> do
                liftIO $ putStrLn "The code you entered has the wrong length!"
                return ResCont
            True  -> do
                if eq then
                    return ResSucc
                 else do
                    liftIO $ putStrLn $ showComp comp
                    return ResCont
 
mainLoop :: PegCode -> Int -> IO (Maybe Int)
mainLoop code n = do
    res <- mainIter code
    case res of
        ResAbort -> return Nothing
        ResCont  -> mainLoop code (n+1)
        ResSucc  -> return $ Just n