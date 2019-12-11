import Data.List
import Data.Function

-- This solution is not pretty. But I can't stand spending more time on this exercise.

main = do
       input <- readFile "10-input"
       let w = 36
           stripped  = filter (/='\n') input
           spacemap  = zipWith (\c i -> ((i`rem`w, i`div`w),c)) stripped [0..]
           asteroids = map fst $ filter (\(_, c) -> c =='#') spacemap
           chosen    = (23,19)
           best      = visibleAsteroids chosen (asteroids\\[chosen])
           helper ((x,y),_) = x*100+y
       print $ helper $ best !! 199

visibleAsteroids :: (Int, Int) -> [(Int, Int)] -> [((Int, Int),KVal) ]
visibleAsteroids a@(ax, ay) asteroids = fixedsrt
    where calcKVal (x1, y1) (x2, y2) = (y2-y1) % (x2-x1)
          kvals    = map (calcKVal a) asteroids
          combined = nubBy (\a b -> snd a == snd b) . sortBy (compare `on` dist . fst) $ zip asteroids kvals
          anglesrt = sortBy (\(p1,_) (p2,_) -> compare (angle p1) (angle p2)) combined
          fixedsrt = (last anglesrt):(init anglesrt)
          dist :: (Int, Int) -> Double
          dist (x2,y2) = sqrt( fromIntegral (x2-ax)^2 + fromIntegral (y2-ay)^2 ) :: Double
          angle :: (Int,Int) -> Double
          angle (x2,y2) = if ax -x2 < 0
                          then -2*pi -atan2 (fromIntegral (ax-x2)) (fromIntegral (ay-y2))
                          else -atan2 (fromIntegral (ax-x2)) (fromIntegral (ay-y2)) 
          

data KVal = KVal { numerator :: Int
                 , denumerator   :: Int
                 }
instance Eq KVal where
    (KVal d n) == (KVal e m) = e==d && n==m
instance Show KVal where
    show (KVal d n) = show d ++ ":" ++ show n

-- My own rational implementation with seperate signs for num and den. Also allows 0 as denom
(%) :: Int -> Int -> KVal
a % b = KVal n d
    where n = a `div` gcd a b
          d = b `div` gcd a b

