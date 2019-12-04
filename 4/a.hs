main = do
    let min = 172851
        max = 675869
    print . length .filter (\s -> risingOrEqual s && containsTwoAdjecent s) $  map show [min..max]

risingOrEqual :: String -> Bool
risingOrEqual []     = True
risingOrEqual [s]    = True
risingOrEqual (s1:s2:sx) = s1 <= s2 && risingOrEqual (s2:sx) 

containsTwoAdjecent :: String -> Bool
containsTwoAdjecent []         = False
containsTwoAdjecent [s]        = False
containsTwoAdjecent (s1:s2:sx) = s1 == s2 || containsTwoAdjecent (s2:sx)
