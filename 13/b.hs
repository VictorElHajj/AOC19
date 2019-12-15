import IntCodeComputer
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM

-- This is also a bit of a mess but I don't have the time to do this properly
-- while studying for the physics exam

main = do
    input <- readFile "13-input"
    let instructions = map read $ splitOn "," input
        indices      = [0..length instructions]
        computer     = run $ Machine 0 0 (IM.insert 0 2 (IM.fromList (zip indices instructions))) [] []
    displayGame $ interpret computer
    gameLoop computer
-- After every input I change the "floor" to be horizontal paddles
-- hackBreakout :: Machine -> Machine


buildMap :: [Int] -> GameMap
buildMap xs = foldl (\acc [x,y,t] -> M.insert (x,y) t acc) M.empty tiles
    where tiles = chunksOf 3 xs

displayMap :: Int -> Int -> GameMap -> [String]
displayMap w h gm = chunksOf 43 $ [(showTile . fromJust) (M.lookup (y,x) gm) | x <- [0..h], y <- [0..w]]

displayScore :: GameMap -> String
displayScore gm = if M.member (-1,0) gm 
                  then "Score: " ++ show ( fromJust (M.lookup (-1,0) gm))
                  else "Score: NaN"

displayGame :: GameMap -> IO ()
displayGame gm = do
                 mapM_ print $ displayMap 42 25 gm
                 print $ displayScore gm

interpret :: Machine -> GameMap
interpret m = buildMap $ reverse $ output m

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

getBallX :: GameMap -> Int
getBallX gm = fst position
    where ballIndex = fromJust $ elemIndex 4 (M.elems gm)
          position  = (M.keys gm)!!ballIndex

getPaddleX :: GameMap -> Int
getPaddleX gm = fst position
    where paddleIndex = fromJust $ elemIndex 3 (M.elems gm)
          position    = (M.keys gm)!!paddleIndex

gameLoop :: Machine -> IO ()
gameLoop computer = do
           let gm  = interpret computer
               direction = signum (getBallX gm - getPaddleX gm)
               ran = runWith computer direction 
           clearScreen 
           displayGame gm
           gameLoop ran

type GameMap = M.Map (Int,Int) Int
data Tile = Empty | Wall | Block | HPaddle | Ball
    deriving (Eq, Enum)

showTile :: Int -> Char
showTile n = case n of
            0 -> ' '
            1 -> '%'
            2 -> '#'
            3 -> '_'
            4 -> '@'
            _ -> error "Illegal tile"
