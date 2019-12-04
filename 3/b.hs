import Data.List.Split
import qualified Data.Map.Lazy as M
main = do
    input <- readFile "3-input"
    let wires = map (splitOn ",") $ splitOn "\n" input
        fstSizes      = map wireToSize $ head wires
        sndSizes      = map wireToSize $ wires !! 1
        fstPoints     = wirePoints 1 (0,0) fstSizes
        sndPoints     = wirePoints 1 (0,0) sndSizes
    print $ minimum . map snd . M.toList $ M.intersectionWith (+) fstPoints sndPoints
    
wireToSize :: String -> (Integer, Integer)
wireToSize ('R':s) = ( read s :: Integer, 0)
wireToSize ('L':s) = (-read s :: Integer, 0)
wireToSize ('U':s) = (0,  read s :: Integer)
wireToSize ('D':s) = (0, -read s :: Integer)
wireToSize _ = error "Unknown wire segment"

-- Builds a hashmap with steps and locations
wirePoints :: Integer -> (Integer,Integer) -> [(Integer, Integer)] -> M.Map (Integer, Integer) Integer
wirePoints _ _ []                      = M.empty
wirePoints n acc ((0,0):xs)            = wirePoints n acc xs
wirePoints n (ax, ay) ((x,0):xs) | x>0 = M.insert (ax+1, ay) n $ wirePoints (n+1) (ax+1, ay) ((x-1,0):xs)
                                 | x<0 = M.insert (ax-1, ay) n $ wirePoints (n+1) (ax-1, ay) ((x+1,0):xs)
wirePoints n (ax, ay) ((0,y):xs) | y>0 = M.insert (ax, ay+1) n $ wirePoints (n+1) (ax, ay+1) ((0,y-1):xs)
                                 | y<0 = M.insert (ax, ay-1) n $ wirePoints (n+1) (ax, ay-1) ((0,y+1):xs)
wirePoints _ _ _ = error "Illegal Argument"
