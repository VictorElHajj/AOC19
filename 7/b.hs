import Data.List.Split
import Data.Maybe (isJust, fromJust)
import Data.List (permutations)
import Data.Sequence (index, Seq, update, fromList)

-- I am not very proud of this solution.

main = do
    input <- readFile "7-input"
    let instructions = fromList $ map read $ splitOn "," input 
        combinations = permutations [5..9]
        -- The below function runs one cycle
        result = maximum $ map (\phase -> ampCycle (setup phase instructions)) combinations
    print result

-- Creates the initial machines, no need to send 0 to A as the default output of E is 0 
setup :: [Int] -> Seq Int -> [Machine]
setup phase ins  = map (\phaseVal -> run (Machine 0 ins (Just phaseVal) 0)) phase

-- Cycles untill halt
ampCycle :: [Machine] -> Int
ampCycle [a,b,c,d,e@(Machine ip_e mem_e _ _)]
    | opcode_e == 99 = output e 
    | otherwise = ampCycle [a2, b2, c2, d2, e2]
    where a2  = run $ Machine (ip a) (memory a) (Just $ output e ) (output a)
          b2  = run $ Machine (ip b) (memory b) (Just $ output a2) (output b)
          c2  = run $ Machine (ip c) (memory c) (Just $ output b2) (output c)
          d2  = run $ Machine (ip d) (memory d) (Just $ output c2) (output d)
          e2  = run $ Machine (ip e) (memory e) (Just $ output d2) (output e)
          ins           = pad 5 $ show $ mem_e `immediate`ip_e
          opcode_e      = read (drop (length ins-2) ins) :: Int
ampCycle _ = error "Illegal argument. Too few phase machines applied."

run :: Machine -> Machine
run m@(Machine ip mem i o) = case opcode of
    -- Add
    01  -> run $ Machine (ip+4) (update (adress 3) (arg 1 + arg 2) mem) i o
    -- Multiply
    02  -> run $ Machine (ip+4) (update (adress 3) (arg 1 * arg 2) mem) i o
    -- Input
    03  -> if isJust i
           then run $ Machine (ip+2) (update (adress 1) (fromJust i) mem) Nothing o
           else m -- Wait for input
    -- Output and halt
    04  -> Machine (ip+2) mem i (arg 1)
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
                       , input        :: Maybe Int
                       , output       :: Int
                       } deriving (Show)

immediate :: Seq Int -> Int -> Int
mem `immediate` i = mem `index` i

pointer :: Seq Int -> Int -> Int
mem `pointer` i = mem `index` (mem `index` i)

pad :: Int -> String -> String
pad n s = replicate (n-length s) '0' ++ s
