import Prelude hiding (Left, Right)
import IntCodeComputer
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Function
import qualified System.Console.ANSI as A
import qualified Data.Map.Lazy as M
import Control.Monad.State

-- This is also a bit of a mess but I don't have the time to do this properly
-- while studying for the physics exam

main = do
    input <- readFile "15-input"
    let instructions = map read $ splitOn "," input
        indices      = [0..length instructions]
        computer     = construct (zip indices instructions)
        robbie       = Robot (0,0) Up computer
        (gm, al)     = exploreMap robbie M.empty M.empty
        graph        = adjToEdges al
        o2pos        = fromJust $ lookup 2 $ map (\(x,y) -> (y,x)) (M.toList gm)
        parentmap    = evalState (searchBF o2pos *> gets snd) (graph, M.insert o2pos o2pos M.empty) 
        furthest     = maximumBy (\pos1 pos2 -> compare (cost pos1 o2pos parentmap) (cost pos2 o2pos parentmap)) (M.keys graph)
    displayMap gm
    -- Part 1
    print $ "Part 1: " ++ show (cost (0,0) o2pos parentmap)
    -- Part 2
    print $ "Part 2: " ++ show (cost furthest o2pos parentmap)

type Adjecent   = M.Map Position [Position]
type Parents    = M.Map Position Position
-- Result list is cost from start
searchBF :: Position -> State (Adjecent, Parents) ()
searchBF p = do
             -- load state
             (adj, parents) <- get
             let neighbors = if p `M.member` adj 
                             then fromJust $ M.lookup p adj :: [Position]
                             else error $ show p
                 new = filter (`M.notMember` parents) neighbors
             mapM_ (\n -> do (adj, parents) <- get
                             put (adj, M.insert n p parents)
                             searchBF n) new

cost :: Position -> Position -> M.Map Position Position -> Integer
cost s g sbf | s == g = 0
             | s /= g = 1 + cost (fromJust $ M.lookup s sbf) g sbf


displayMap :: GameMap -> IO ()
displayMap gm | M.size gm == 0 = return ()
displayMap gm = do
                    A.clearScreen
                    let maxX = fst $ maximumBy (compare `on` fst) (M.keys gm)
                        minX = fst $ minimumBy (compare `on` fst) (M.keys gm)
                        maxY = snd $ maximumBy (compare `on` snd) (M.keys gm)
                        minY = snd $ minimumBy (compare `on` snd) (M.keys gm)
                        w = maxX - minX
                        h = maxY - minY
                        coords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]\\[(0,0)]
                    A.setCursorPosition (0-minY) ( 0-minX)
                    putChar 'X' -- Print start
                    mapM_ (\(x,y) -> do
                                     A.setCursorPosition (y-minY) (x-minX)
                                     putChar (showTile $ M.findWithDefault 9 (x,y) gm)) coords
                    putStrLn ""

displayRobot :: Position -> IO ()
displayRobot (x,y) = do
                   A.setCursorPosition x y
                   putChar '@'

-- Left hand rule maze exploration
exploreMap :: Robot -> GameMap -> AdjacancyList -> (GameMap, AdjacancyList)
exploreMap (Robot pos dir computer) gm al =
                                         if fullyExplored al
                                         then (gm, al)
                                         else do
                                         --displayMap gm (Robot pos dir computer)
                                         let l        = runWith computer (fromEnum $ pred dir)
                                             tileVal  = head (output l)
                                             newPos   = addDir pos dir
                                             tileAdj  = M.findWithDefault (defaultAdj pos) pos al
                                             tileAdj' = M.insert newPos tileVal tileAdj
                                             newAl    = M.insert pos tileAdj' al
                                         -- If we move left and encounter a wall
                                         if tileVal  == 0
                                            -- rotate right and try again
                                            then let wallPos = addDir pos dir
                                                     newMap  = M.insert wallPos 0 gm
                                                    in exploreMap (Robot pos (succ dir) l) newMap newAl
                                            -- if we dont hit wall keep going
                                            -- if newPos is already in our map, finish exploration
                                            else let newPos2 = addDir pos dir
                                                     newMap = M.insert newPos2 tileVal gm
                                                 in exploreMap (Robot newPos2 (pred dir) l) newMap newAl
addDir :: Position -> Direction -> Position
addDir (x,y) Up = (x,y+1)
addDir (x,y) Right = (x+1,y)
addDir (x,y) Down = (x,y-1)
addDir (x,y) Left = (x-1,y)

defaultAdj :: Position -> M.Map Position Int
defaultAdj p = M.fromList [ (addDir p Up,9)
                          , (addDir p Right,9)
                          , (addDir p Down,9)
                          , (addDir p Left,9) ]

-- Removes walls from adjacancy list
adjToEdges :: AdjacancyList -> M.Map Position [Position]
adjToEdges al = M.fromList $ map (\(p, al2) -> (p, M.keys $ M.filter (\x -> x == 1 || x == 2) al2)) al'
    where al' = M.toList al

fullyExplored :: AdjacancyList -> Bool
fullyExplored a | M.null a = False
                | otherwise = (not . or) $ map (==9) $ concatMap M.elems (M.elems a)

type AdjacancyList = M.Map Position (M.Map Position Int)
type GameMap = M.Map Position Int
data Tile = Empty | Wall | O2System
    deriving (Eq, Enum)

type Position = (Int, Int)
data Direction = Up | Right | Down | Left
instance Enum Direction where
    fromEnum Up    = 1
    fromEnum Down  = 2
    fromEnum Left  = 3
    fromEnum Right = 4
    succ Up    = Right
    succ Right = Down
    succ Down  = Left
    succ Left  = Up
    pred Up    = Left
    pred Left  = Down
    pred Down  = Right
    pred Right = Up
data Robot = Robot Position Direction Machine

showTile :: Int -> Char
showTile n = case n of
            0 -> '█' -- Wall
            1 -> ' ' -- Empty
            2 -> '#' -- Oxygen syste,
            9 -> '░' -- Unknown
            _ -> error "Illegal tile"
