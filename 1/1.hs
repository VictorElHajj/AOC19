main = do
    input <- readFile "1-input"
    let masses = map read (lines input) :: [Integer]
        fuel = sum $ map (subtract 2 . flip div 3) masses
    putStrLn $ show fuel

massToFuel = (subtract 2 . flip div 3)
