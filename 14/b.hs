import Data.List.Split
import Data.Maybe
import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy
main = do
    input <- readFile "14-input"
        -- All possible reactions
    let parsed = map (parse . words) $ lines $ filter (`notElem`",=>") input
        -- Create a hashmap where the key is the name of the ouput chemical and the value the reaction
        reactions = foldl (\acc r -> M.insert ((name . output) r) r acc) M.empty parsed
        findBSBounds :: Integer -> (Integer,Integer)
        findBSBounds n = if tryFuel n reactions < 1000000000000
                then findBSBounds (n*2)
                else (n`div`2,n)
        searchBS :: Integer -> Integer -> Integer
        -- Binary search between the bounds found in the previous function
        searchBS lb ub   | tryFuel (mid-1) reactions < 1000000000000 && tryFuel mid reactions > 1000000000000 = mid-1
                         | otherwise = if tryFuel mid reactions < 1000000000000
                                      then searchBS (mid+1) ub 
                                      else searchBS lb (mid-1)
                        where mid = (lb+ub) `div` 2
        (lb,ub) = findBSBounds 1
    print $ searchBS lb ub

tryFuel :: Integer -> ReactionBook -> Integer
tryFuel n reactions = evalState (cost (Chem n "FUEL") *> (gets $ (M.findWithDefault 0 "ORE" . fst))) (M.insert "ORE" 0 M.empty ,reactions)

ingredientsAndWaste :: Chemical -> M.Map String Reaction  -> (Integer, [Chemical])
ingredientsAndWaste (Chem n c) list = (waste, map (\(Chem a name) -> Chem (a*cycles) name) ingredients)
    where (Reaction ingredients output) = fromJust $ M.lookup c list
          cycles = (1 + ((n-1) `div` (amount output)))
          waste  = cycles * (amount output) - n

type ReactionState = (M.Map String Integer, ReactionBook)

cost :: Chemical -> State ReactionState ()
cost (Chem n "ORE") = do
                      (tally,rb) <- get
                      put (M.insertWith (+) "ORE" n tally, rb)
cost (Chem n a  )   = do
                      (tally,rb) <- get
                      let extra = M.findWithDefault 0 a tally
                      if extra >= n
                      then put (M.insert a (extra-n) tally, rb)
                      else do
                        let (waste, ingredients) = ingredientsAndWaste (Chem (n-extra) a) rb
                        -- Replace excess with new waste
                        put (M.insert a waste tally, rb)
                        mapM_ cost ingredients




parse :: [String] -> Reaction
parse list = Reaction ins out
    where pairs = chunksOf 2 list
          out = pairToChem (last pairs)
          ins = map pairToChem (init pairs)
          pairToChem :: [String] -> Chemical
          pairToChem s = Chem a n
            where a = read (s!!0)
                  n = s!!1

type ReactionBook = M.Map String Reaction

data Chemical = Chem { amount :: Integer
                     , name   :: String
                     } deriving Eq
data Reaction = Reaction { inputs :: [Chemical]
                         , output :: Chemical
                         } deriving (Eq,Show)
instance Show Chemical where
    show (Chem a n) = show a ++ " " ++ n

