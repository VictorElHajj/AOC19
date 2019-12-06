import Data.List.Split
import qualified Data.Map.Lazy as M
import Data.Maybe (isJust, fromJust)
main = do
    input <- readFile "6-input"
    let orbits = map (\(p:m:[]) -> (p,m)) $ map (splitOn ")") $ lines input
        direct = length orbits
        orbitmap = populate orbits
        indir  = sum $ map (\(a,_) -> indirect a orbitmap) orbits
    print (direct + indir)

populate :: [(String, String)] -> M.Map String String
populate [] = M.empty
populate ((body,sattelite):xs) = M.insert sattelite body $ populate xs

indirect :: String -> M.Map String String -> Int
indirect s m = if isJust (M.lookup s m)
               then 1 + indirect (fromJust (M.lookup s m)) m
               else 0
