import Data.List.Split
import Data.Sequence (index, Seq, update, fromList)
import Data.Foldable

main = do
    input <- readFile "5-input"
    let instructions = fromList $ map read $ splitOn "," input
    result <- program 0 instructions
    return ()

program ::Int -> Seq Int -> IO (Seq Int)
program n mem
    | opcode ins 1  = program (n+4) $ update (adress 3) (arg 1 + arg 2) mem
    | opcode ins 2  = program (n+4) $ update (adress 3) (arg 1 * arg 2) mem
    | opcode ins 3  = do
                      input <- getLine
                      let parsed = read input :: Int
                      program (n+2) $ update (adress 1) parsed mem
    | opcode ins 4  = do
                      print $ arg 1
                      program (n+2) mem
    | opcode ins 99 = return mem
    | otherwise     = error $ "n: " ++ show n ++ " op: " ++ ins
    where ins         = pad 5 $ show $ mem `immediate` n
          param       = take (length ins -2) ins
          arg i       = if (param!!(3-i)) == '1' 
                        then mem `immediate` (n+i)
                        else mem `pointer` (n+i)
          adress i    = mem `immediate` (n+i)

opcode :: String -> Int -> Bool
opcode ins op  = let val = read $ (drop $ length ins-2) ins
                 in  val == op

immediate :: Seq Int -> Int -> Int
mem `immediate` i = mem `index` i

pointer :: Seq Int -> Int -> Int
mem `pointer` i = mem `index` (mem `index` i)

pad :: Int -> String -> String
pad n s = replicate (n-length s) '0' ++ s
