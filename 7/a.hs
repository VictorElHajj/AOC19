import Data.List.Split
import Data.List (permutations)
import Data.Sequence (index, Seq, update, fromList)

main = do
    input <- readFile "7-input"
    let instructions = fromList $ map read $ splitOn "," input 
        combinations = permutations [0,1,2,3,4]
        -- The below function is a bit messy. It runs one sequene of 4 amps. The head is the result of the last intcode program.
        program test = (head . output . head) $ foldl (\(m:acc) a -> run (Machine 0 instructions [a,head (output m)] []) : (m:acc)) [run (Machine 0 instructions [head test, 0] [])]  (tail test)
        result = maximum (map program combinations) :: Int
    print result

-- N is the instruction pointer. Mem is the shared memory/instructions
run :: Machine -> Machine
run m@(Machine ip mem i o) = case opcode of
    -- Add
    01  -> run $ Machine (ip+4) (update (adress 3) (arg 1 + arg 2) mem) i o
    -- Multiply
    02  -> run $ Machine (ip+4) (update (adress 3) (arg 1 * arg 2) mem) i o
    -- Input
    03  -> run $ Machine (ip+2) (update (adress 1) (head i) mem) (tail i) o
    -- Output
    04  -> run $ Machine (ip+2) mem i (arg 1 : o)
    -- Jump if true
    05 -> if arg 1 /= 0 
                     then run $ Machine (arg 2) mem i o
                     else run $ Machine (ip +3) mem i o
    -- Jump if false
    06 -> if arg 1 == 0
                     then run $ Machine (arg 2) mem i o
                     else run $ Machine (ip +3) mem i o
    -- Test less than
    07 -> if arg 1 < arg 2
                     then run $ Machine (ip+4) (update (adress 3) 1 mem) i o
                     else run $ Machine (ip+4) (update (adress 3) 0 mem) i o
    -- Test equal
    08 -> if arg 1 == arg 2
                     then run $ Machine (ip+4) (update (adress 3) 1 mem) i o
                     else run $ Machine (ip+4) (update (adress 3) 0 mem) i o
    -- Halt run 
    99 -> m
    -- Error handling
    _  -> error $ "ip: " ++ show ip ++ " op: " ++ ins
    where ins         = pad 5 $ show $ mem `immediate` ip
          opcode      = read (drop (length ins-2) ins) :: Int
          param       = take (length ins -2) ins
          arg i       = if (param!!(3-i)) == '1' 
                        then mem `immediate` (ip+i)
                        else mem `pointer` (ip+i)
          adress i    = mem `immediate` (ip+i) -- Needed because adress is always immediate

data Machine = Machine { ip           :: Int
                       , memory       :: Seq Int
                       , input        :: [Int]
                       , output       :: [Int]
                       } 

immediate :: Seq Int -> Int -> Int
mem `immediate` i = mem `index` i

pointer :: Seq Int -> Int -> Int
mem `pointer` i = mem `index` (mem `index` i)

pad :: Int -> String -> String
pad n s = replicate (n-length s) '0' ++ s
