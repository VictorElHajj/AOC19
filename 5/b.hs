import Data.List.Split
import Data.Sequence (index, Seq, update, fromList)

main = do
    input <- readFile "5-input"
    let instructions = fromList $ map read $ splitOn "," input 
        machine      = Machine 0 instructions [5] []
        result = program machine
    print $ output result

-- N is the instruction pointer. Mem is the shared memory/instructions
program :: Machine -> Machine
program m@(Machine ip mem i o) = case opcode of
    -- Add
    01  -> program $ Machine (ip+4) (update (adress 3) (arg 1 + arg 2) mem) i o
    -- Multiply
    02  -> program $ Machine (ip+4) (update (adress 3) (arg 1 * arg 2) mem) i o
    -- Input
    03  -> program $ Machine (ip+2) (update (adress 1) (head i) mem) (tail i) o
    -- Output
    04  -> program $ Machine (ip+2) mem i (arg 1 : o)
    -- Jump if true
    05 -> if arg 1 /= 0 
                     then program $ Machine (arg 2) mem i o
                     else program $ Machine (ip +3) mem i o
    -- Jump if false
    06 -> if arg 1 == 0
                     then program $ Machine (arg 2) mem i o
                     else program $ Machine (ip +3) mem i o
    -- Test less than
    07 -> if arg 1 < arg 2
                     then program $ Machine (ip+4) (update (adress 3) 1 mem) i o
                     else program $ Machine (ip+4) (update (adress 3) 0 mem) i o
    -- Test equal
    08 -> if arg 1 == arg 2
                     then program $ Machine (ip+4) (update (adress 3) 1 mem) i o
                     else program $ Machine (ip+4) (update (adress 3) 0 mem) i o
    -- Halt program 
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
