main = do
    input <- readFile "1-input"
    let masses = map read (lines input) :: [Integer]
        fuel = sum $ map massToFuel masses
    putStrLn $ show fuel

massToFuel :: Integral a => a -> a
massToFuel m    | fuel <= 0 = 0
                | otherwise = fuel + massToFuel fuel
            where fuel = (flip (-) 2 . flip div 3) m
