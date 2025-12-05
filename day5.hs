import Data.Char
import Data.Ix
import Text.ParserCombinators.ReadP

import Data.List
import Control.Arrow

parseRanges = endBy ((,) <$> num <*> (char '-' *> num)) eol

parseNums = endBy num eol

num :: ReadP Int
num = read <$> munch1 isDigit

eol = char '\n'

parse = fst . last . readP_to_S ((,) <$> parseRanges <*> (eol *> parseNums))

part1 rl = length . filter isValid
 where
  isValid n = any (`inRange` n) rl

overlap a@(a1, a2) b@(b1, b2)
  | a1 > b1 = overlap b a
  | a1 < b1 && a2 < b1 = False
  | otherwise = True

join a@(a1, a2) b@(b1, b2)
  | a1 > b1 = join b a
  | a1 <= b1 && a2 >= b2 = a
  | a1 < b1 && a2 < b1 = undefined
  | otherwise = (a1, b2)

part2 rl _ = sum $ rangeSize <$> foldl' go mempty rl
  where
    go l r = foldl' join r joinable : disjoint
      where
        (joinable, disjoint) = partition (overlap r) l

main = getContents >>= print . (uncurry part1 &&& uncurry part2) . parse
