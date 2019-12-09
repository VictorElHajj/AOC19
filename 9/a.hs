import Data.List.Split
import Data.IntMap.Lazy (IntMap, fromList, insert, member, (!))

main = do
    input <- readFile "9-input"
    let instructions = map read $ splitOn "," input
        indexes      = [0..length instructions]
        hashedInst   = fromList $ zip indexes instructions :: IntMap Int
        result     = run $ Machine 0 0 hashedInst [1] []
    print $ reverse $ output result

run :: Machine -> Machine
run m@(Machine ip rb mem i o) = case opcode of
    -- Add
    01  -> run $ Machine (ip+4) rb (insert (adress 3) (arg 1 + arg 2) mem) i o
    -- Multiply
    02  -> run $ Machine (ip+4) rb (insert (adress 3) (arg 1 * arg 2) mem) i o
    -- Input
    03  -> if null i then error "No input"
           else run $ Machine (ip+2) rb (insert (adress 1) (head i) mem) (tail i) o
    -- Output
    04  -> run $ Machine (ip+2) rb mem i (arg 1 : o)
    -- Jump if true
    05 -> if arg 1 /= 0
                     then run $ Machine (arg 2) rb mem i o
                     else run $ Machine (ip+3) rb mem i o
    -- Jump if false
    06 -> if arg 1 == 0
                     then run $ Machine (arg 2) rb mem i o
                     else run $ Machine (ip+3) rb mem i o
    -- Test less than
    07 -> if arg 1 < arg 2
                     then run $ Machine (ip+4) rb (insert (adress 3) 1 mem) i o
                     else run $ Machine (ip+4) rb (insert (adress 3) 0 mem) i o
    -- Test equal
    08 -> if arg 1 == arg 2
                     then run $ Machine (ip+4) rb (insert (adress 3) 1 mem) i o
                     else run $ Machine (ip+4) rb (insert (adress 3) 0 mem) i o
    09 -> run $ Machine (ip+2) (rb + arg 1) mem i o
    -- Halt run 
    99 -> m
    -- Error handling
    _  -> error $ "ip: " ++ show ip ++ " op: " ++ ins ++ " rb: " ++ show rb ++ " inst: " ++ show mem
    where ins         = pad 5 $ show $ mem `immediate` ip
          opcode      = read (drop (length ins-2) ins) :: Int
          param       = take (length ins -2) ins
          arg n   = case param!!(3-n) of
                        '1' -> mem `immediate` (ip+n)
                        '0' -> mem `pointer`   (ip+n)
                        '2' -> mem `immediate` (rb + mem `immediate` (ip+n))
                        _   -> error "Unknown parameter in argument"
          adress n = case param!!(3-n) of
                        '1' -> mem `immediate` (ip+n) -- Needed because adress is always immediate
                        '0' -> mem `immediate` (ip+n) -- Needed because padding defaults to 0s 
                        '2' -> rb + mem `immediate` (ip+n)
                        _   -> error "Unknown parameter in adress"

data Machine = Machine { ip           :: Int
                       , relBase      :: Int
                       , memory       :: IntMap Int
                       , input        :: [Int]
                       , output       :: [Int]
                       } deriving (Show)

immediate :: IntMap Int -> Int -> Int
mem `immediate` i = if member i mem
                    then mem ! i
                    else 0

pointer :: IntMap Int -> Int -> Int
mem `pointer` i = if member i mem
                  then mem `immediate` (mem ! i)
                  else mem `immediate` 0

pad :: Int -> String -> String
pad n s = replicate (n-length s) '0' ++ s
