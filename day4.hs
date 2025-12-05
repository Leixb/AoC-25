module Main where

import Data.Array.Unboxed
import Control.Arrow
import Data.Foldable

type Coord = (Int, Int)
type Diagram = UArray Coord Char

moves :: Coord -> [Coord]
moves pos = (.+. pos) <$> deltas
  where
    deltas = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], not (x == 0 && y == 0)]
    (ax, ay) .+. (bx, by) = (ax + bx, ay + by)

parse :: String -> Diagram
parse s = listArray ((1, 1), (n, m)) $ concat l
  where
    l = lines s
    n = length l
    m = length $ head l

isRoll = (== '@')
numRolls = length . filter isRoll

neighbors d p = (d !) <$> filter (inRange (bounds d)) (moves p)

removable d = filter ((<4) . numRolls . neighbors d . fst) . filter (isRoll . snd) $ assocs d

part1 :: Diagram -> Int
part1 = length . removable

part2 d = fmap ((initial -) . fst) . find (uncurry (==)) $ zip stages (tail stages)
  where
    initial = numRolls $ elems d
    stages = numRolls . elems <$> iterate (\x -> x // toX (removable x)) d
    toX = fmap (second (const 'x'))

main = getContents >>= print . (part1 &&& part2) . parse
