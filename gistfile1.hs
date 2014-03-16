check :: IterRes -> Bool -> Either IterRes IterRes
check res False = Left res
check res True  = Right res

prettyCount :: Int -> String
prettyCount 1 = "first"
prettyCount 2 = "second"
prettyCount 3 = "third"
prettyCount n = (show n) ++ "th"

mainLoop :: PegCode -> Int -> IO (Maybe Int)
mainLoop code n = do
    printf "This is your %s try. Your guess please:\n" $ prettyCount n
    
    res <- runEitherT $ do
        str <- liftIO $ getLine
        -- liftIO $ putStrLn ""
        hoistEither $ check ResAbort $ str /= "quit"

        let guess = strToPegs str
            eq   = code == guess
            comp = compPegs code guess

        hoistEither $ check ResWrongInput $ length guess == length code
        hoistEither $ check ResFound $ not eq

        return comp

    case res of
        Right comp -> do
            putStrLn $ "Here is your hint: " ++ showComp comp
            mainLoop code (n+1)
        Left ResAbort -> return Nothing
        Left ResWrongInput -> do
            putStrLn "The code you entered has the wrong length!"
            mainLoop code n
        Left ResFound  -> return $ Just n