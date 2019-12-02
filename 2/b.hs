import Data.List.Split
import Data.Sequence
import Data.Foldable
import System.Environment
main = do
    input <- readFile "2-input"
    args <- getArgs
    let instructions = fromList $ map read $ splitOn "," input
        fixedArgs = map read args :: [Int]
        fixedInstructions = update 2 (fixedArgs!!1) $ update 1 (fixedArgs!!0) instructions
        result = program 0 fixedInstructions
    putStrLn . show . head $ toList result

program ::Int -> Seq Int -> Seq Int
program n mem
    | instruction == 1  = program (n+4) $ update adress (a+b) mem
    | instruction == 2  = program (n+4) $ update adress (a*b) mem
    | otherwise         = mem
    where instruction = mem `index` n
          a           = mem `index` (mem `index` (n+1))
          b           = mem `index` (mem `index` (n+2))
          adress      = mem `index` (n+3)

-- From CLI testing: Result is Adress 2 + 275620 + Adress 1 * 245760
-- (19690720 - 245760)/245760 = 79.00002 => Adress 1 = 79
-- Running with 79 0 gives 19690660. 19690720-19690660 = 60 => Adress 2 = 60
-- Answer = 79*10+60=850
