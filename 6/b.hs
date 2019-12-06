import Data.List.Split
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe (isJust, fromJust)
main = do
    input <- readFile "6-input"
    let orbits = map (\(p:m:[]) -> (p,m)) $ map (splitOn ")") $ lines input
        orbitmap = populate orbits
        minsteps = subtract 2 $ length $ stepsToRoot "YOU" orbitmap `symmetricDifference` stepsToRoot "SAN" orbitmap
    print minsteps

populate :: [(String, String)] -> M.Map String String
populate [] = M.empty
populate ((body,sattelite):xs) = M.insert sattelite body $ populate xs

stepsToRoot :: String -> M.Map String String -> [String]
stepsToRoot s m = if isJust (M.lookup s m)
                  then s : stepsToRoot (fromJust (M.lookup s m)) m
                  else []

symmetricDifference :: Eq a => [a] -> [a] -> [a]
symmetricDifference a b = (\\) (a `union` b) (a `intersect` b)
