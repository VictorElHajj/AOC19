import Data.List

main = do
    let planets = [ Moon (Position   1    2  (-9)) (Velocity 0 0 0)
                  , Moon (Position (-1) (-9) (-4)) (Velocity 0 0 0)
                  , Moon (Position  17    6    8)  (Velocity 0 0 0)
                  , Moon (Position  12    4    2)  (Velocity 0 0 0) ]
        example = [ Moon (Position (-1) 0 2) (Velocity 0 0 0),
                    Moon (Position 2 (-10) (-7)) (Velocity 0 0 0),
                    Moon (Position 4 (-8) 8) (Velocity 0 0 0),
                    Moon (Position 3 5 (-1)) (Velocity 0 0 0) ]
        example2 = [ Moon (Position (-8) (-10) 0) (Velocity 0 0 0),
                    Moon (Position 5 5 10) (Velocity 0 0 0),
                    Moon (Position 2 (-7) 3) (Velocity 0 0 0),
                    Moon (Position 9 (-8) (-3)) (Velocity 0 0 0) ]
        simulated = iterate simulate planets
    print $ sum $ map energy (simulated!!1000)

energy :: Moon -> Integer
energy (Moon p v) = potential*kinetic
    where potential = abs (x p)  + abs (y p)  + abs (z p)
          kinetic   = abs (vx v) + abs (vy v) + abs (vz v)

simulate :: [Moon] -> [Moon]
simulate ps = map (ps`calc`) ps
    where calc :: [Moon] -> Moon -> Moon
          calc ms m = Moon (Position (x (pos m) + vx newVel) (y (pos m) + vy newVel) (z (pos m) + vz newVel)) newVel
            where newVel = addVel (vel m) $ foldl1 addVel  $ map force rest
                  rest = ms \\ [m]
                  force :: Moon -> Velocity
                  force m2 = Velocity x2 y2 z2
                    where p  = pos m
                          p2 = pos m2
                          x2 = if (x p2 - x p) /= 0
                               then (x p2 - x p) `div` abs (x p2 - x p)
                               else 0
                          y2 = if (y p2 - y p) /= 0
                               then (y p2 - y p) `div` abs (y p2 - y p)
                               else 0
                          z2 = if (z p2 - z p) /= 0
                               then (z p2 - z p) `div` abs (z p2 - z p)
                               else 0

data Position = Position
            { x :: Integer
            , y :: Integer
            , z ::Integer
            } deriving (Show, Eq)
data Velocity = Velocity
            { vx :: Integer
            , vy :: Integer
            , vz ::Integer
            } deriving (Show, Eq)
addVel :: Velocity -> Velocity -> Velocity
addVel v1 v2 = Velocity (vx v1 + vx v2) (vy v1 + vy v2) (vz v1 + vz v2)

data Moon = Moon
         { pos :: Position
         , vel :: Velocity
         } deriving (Show, Eq)

