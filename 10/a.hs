import Data.List
import Data.Function
main = do
       input <- readFile "10-input"
       let w = 36
           stripped  = filter (/='\n') input
           spacemap  = zipWith (\c i -> ((i`rem`w, i`div`w),c)) stripped [0..]
           asteroids = map fst $ filter (\(_, c) -> c =='#') spacemap
           best      = zip asteroids $ map (\asteroid -> visibleAsteroids asteroid (asteroids\\[asteroid])) asteroids
       print $ maximumBy (compare `on` snd) best

visibleAsteroids :: (Int, Int) -> [(Int, Int)] -> Int
visibleAsteroids asteroid asteroids = length $ nub (map (calcKVal asteroid) asteroids)
    where calcKVal :: (Int, Int) -> (Int, Int) -> KVal
          calcKVal (x1, y1) (x2, y2) = (y2-y1) % (x2-x1)

data KVal = KVal { denominator :: Int
                 , numerator   :: Int
                 } deriving (Show, Eq)

-- My own rational implementation with seperate signs for num and den. Also allows 0 as denom
(%) :: Int -> Int -> KVal
a % b = KVal d n
    where d = a `div` gcd a b
          n = b `div` gcd a b
