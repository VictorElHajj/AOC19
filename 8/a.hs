import Data.List.Split
import Data.List (minimumBy)
import Data.Function(on)

main = do
    input <- readFile "8-input"
    let width   = 25
        height  = 6
        layers  = chunksOf (width*height) (tail input) -- Last char is a new line
        fewest0 = minimumBy (compare `on` occurences (=='0')) layers
        answer  = occurences (=='1') fewest0 * occurences (=='2') fewest0
    print answer


occurences :: Eq a => (a -> Bool) -> [a] -> Int
occurences f = length . filter f
