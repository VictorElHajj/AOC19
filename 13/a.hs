import IntCodeComputer
import Data.List.Split
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Map.Lazy as M
main = do
    input <- readFile "13-input"
    let instructions = map read $ splitOn "," input
        indices      = [0..length instructions]
        computer     = run $ construct $ zip indices instructions 
        cOutput      = reverse $ output $ run computer
        gameMap      = buildMap cOutput 
        width        = fst $ maximumBy (compare `on` fst) (M.keys gameMap)
        height       = snd $ maximumBy (compare `on` snd) (M.keys gameMap)
    --mapM_ print $ displayMap width height gameMap 
    print $ length $ filter (==Block) (M.elems gameMap)

buildMap :: [Int] -> GameMap
buildMap xs = foldl (\acc [x,y,t] -> M.insert (x,y) (toEnum t) acc) M.empty tiles
    where tiles = chunksOf 3 xs

displayMap :: Int -> Int -> GameMap -> [String]
displayMap w h gm = chunksOf 43 $ concat [(show . fromJust) (M.lookup (y,x) gm) | x <- [0..h], y <- [0..w]]

type GameMap = M.Map (Int,Int) Tile
data Tile = Empty | Wall | Block | HPaddle | Ball
    deriving (Eq, Enum)
instance Show Tile
    where show Empty   = " "
          show Wall    = "%"
          show Block   = "#"
          show HPaddle = "H"
          show Ball    = "@"
