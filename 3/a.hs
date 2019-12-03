import Data.List.Split
import Data.List
main = do
    input <- readFile "3-input"
    let wires = map (splitOn ",") $ splitOn "\n" input
        fstSizes      = map wireToSize $ head wires
        sndSizes      = map wireToSize $ wires !! 1
        fstPoints     = wirePoints fstSizes
        sndPoints     = wirePoints sndSizes
    print $ minimum $ map manhattanDistance $ intersect fstPoints sndPoints

wireToSize :: String -> (Integer, Integer)
wireToSize ('R':s) = ( read s :: Integer, 0)
wireToSize ('L':s) = (-read s :: Integer, 0)
wireToSize ('U':s) = (0,  read s :: Integer)
wireToSize ('D':s) = (0, -read s :: Integer)
wireToSize _ = error "Unknown wire segment"

-- Range that does not return empty list of a>b
range a b | a<b = [a..b]
          | a>b = [b..a]
          | otherwise = [a]

-- Manhattan distance from center
manhattanDistance :: (Integer, Integer) -> Integer
manhattanDistance (x, y) = (abs x) + (abs y)

-- Traverse and save all points of the wire
wirePoints :: [(Integer, Integer)] -> [(Integer, Integer)]
wirePoints = delete (0,0) . foldl (\acc@((ax, ay):_) (x, y) -> (ax+x, ay+y):(acc++[(a, b) | a <- range ax (ax+x), b <- range ay (ay+y)])) [(0,0)] 
