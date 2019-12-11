import Prelude hiding (Right, Left)
import IntCodeComputer
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Lazy as M

main = do
    input <- readFile "11-input"
    let instructions = Prelude.map read $ splitOn "," input
        indexes      = [0..length instructions]
        computer     = construct $ zip indexes instructions
        board        = M.empty :: M.Map Position Color
        robbie       = Robot (0,0) Up computer
        result       = paint robbie
    print $ length $ paint robbie board

paint :: Robot -> Board -> Board
paint (Robot _ _ Halt) board = board
paint (Robot inPos@(x,y) inDir machine) board | result == Halt = board
                                              | otherwise      = paint (Robot outPos outDir result) outBoard
    where inColor   = get inPos board
          result    = runWith machine (fromEnum inColor)
          outColor  = toEnum (output result!!1) :: Color
          outBoard  = set inPos outColor board
          outDir    = case (head (output result)) of
                        1 -> pred inDir
                        0 -> succ inDir
          outPos    = case outDir of
                        Up    -> (x,y-1)
                        Right -> (x+1,y)
                        Down  -> (x,y+1)
                        Left  -> (x-1,y)

data Color = Black | White deriving (Show, Enum)
type Board = M.Map (Int, Int) Color
type Position = (Int,Int)
data Direction = Up | Right | Down | Left
instance Enum Direction
    where
        succ Up = Right
        succ Right = Down
        succ Down = Left
        succ Left = Up
        pred Up = Left
        pred Left = Down
        pred Down = Right
        pred Right = Up
data Robot = Robot Position Direction Machine

-- Default to black
get :: Position -> Board  -> Color
get p m = if p `M.member` m
            then fromJust $ p `M.lookup` m
            else Black
set :: (Int, Int) -> Color -> Board -> Board
set = M.insert


