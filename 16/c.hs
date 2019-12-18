module Main where


import Data.List (scanl')
import Data.Char (digitToInt, isDigit)
import Control.Monad (when)


cumSum :: Num a => [a] -> [a]
cumSum = tail . scanl' (+) 0


f :: (Num a, Integral a) => [a] -> [a]
f = map (\n -> (abs n) `mod` 10) . reverse . cumSum . reverse


compN :: Int -> (a -> a) -> (a -> a)
compN n f = head . drop n . iterate f


main :: IO ()
main = do
  let input = "03036732577212944063491565474664"
  let digits = map ((fromIntegral.digitToInt) :: Char -> Int) . filter (isDigit) $ input
  let messageOffset = (read :: String -> Int) . concatMap show . take 7 $ digits
  when (messageOffset <= length(digits) `div` 2) (error "offset too small")
  putStrLn . concatMap show . take 8 . compN 100 f . drop messageOffset . concat . replicate 10000 $ digits
