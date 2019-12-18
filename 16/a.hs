import Data.Char (digitToInt)

main = do
    input <- readFile "16-input"
    let fix = map digitToInt $ filter (/='\n') input
    print $ take 8 $ (iterate fft fix)!!100

fft :: [Int] -> [Int]
fft input = zipWith (\pattern list -> (abs . sum) (zipWith (\f n -> f n)  pattern list) `mod` 10) patterns (replicate (length input) input)  
    where patternN n = tail . cycle . concat $ map (replicate n) [(*0), (*1), (*0), (*(-1))]
          patterns   = map patternN [1..]
