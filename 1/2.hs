main = do
    input <- readFile "1-input"
    let masses = map read (lines input) :: [Integer]
        fuel = sum $ map massToFuel masses
    putStrLn $ show fuel

massToFuel :: Integral a => a -> a
massToFuel = sum . takeWhile (>0) . iterate fuel . fuel
            where fuel = (subtract 2 . flip div 3)
