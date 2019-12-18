import Data.Char (digitToInt,chr,isDigit)
import Data.Int

main = do
    input <- readFile "16-input"
    let firstInput = map digitToInt . filter isDigit $ input :: [Int]
        largeInput = (concat . replicate 10000) firstInput
        offset     = read $ concatMap show $ take 7 firstInput
    print $ "Offset: " ++ show offset
    print $ concatMap show $ take 8 $ applyN fft 100 (drop offset largeInput)

fft :: [Int] -> [Int]
fft input = sndHalf
    where sndHalf    = map (\n -> abs n`mod`10) $ reverse $ tail . scanl (+) 0  $ reverse input

applyN :: (a -> a) -> Int -> (a -> a)
applyN f n = foldl1 (.) (replicate n f)

