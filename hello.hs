main = do
    putStrLn "Hello everyone!"
    putStrLn ("This is my favorite odd numbers:" ++ show (filter odd [1..10]))