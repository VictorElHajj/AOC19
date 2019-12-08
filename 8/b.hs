import Data.List.Split

main = do
    input <- readFile "8-input"
    let width   = 25
        height  = 6
        layers  = chunksOf (width*height) (init input) -- Last char is a new line
        result  = foldl1 addLayer layers
        -- Easier to see than 1s and 0s
        readable = map (\c -> if c == '1' then '█' else ' ') result
    mapM_ putStrLn (chunksOf width readable)

addLayer :: String -> String -> String
a `addLayer` b = zipWith (\charA charB -> if charA == '2' then charB else charA) a b
