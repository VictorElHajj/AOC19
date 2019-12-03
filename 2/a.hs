import Data.List.Split
import Data.Sequence
import Data.Foldable

main = do
    input <- readFile "2-input"
    let instructions = fromList $ map read $ splitOn "," input
        fixedInstructions = update 2 2 $ update 1 12 instructions
        result = program 0 fixedInstructions
    print . head $ toList result

program ::Int -> Seq Int -> Seq Int
program n mem
    | instruction == 1  = program (n+4) $ update adress (a+b) mem
    | instruction == 2  = program (n+4) $ update adress (a*b) mem
    | instruction == 99 = mem
    | otherwise         = error "Unknown instruction"
    where instruction = mem `index` n
          a           = mem `index` (mem `index` (n+1))
          b           = mem `index` (mem `index` (n+2))
          adress      = mem `index` (n+3)

