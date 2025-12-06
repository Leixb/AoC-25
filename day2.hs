import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

parser :: ReadP [(Int, Int)]
parser = sepBy ((,) <$> (int <* char '-') <*> int) (char ',')
 where
  int :: ReadP Int
  int = read <$> munch1 isDigit

parse = fst . last . readP_to_S parser

numDigits = length . show

next n
  | even digits = if lower > upper then succ upper else upper
  | otherwise = tenPow
 where
  upper = n `div` tenPow
  lower = n `mod` tenPow
  digits = numDigits n
  tenPow = 10 ^ (digits `div` 2)

toInvalid n = n * succ tenPow
 where
  digits = numDigits n
  tenPow = 10 ^ digits

genList (l, u) = takeWhile (<= u) . fmap toInvalid $ [(next l) ..]

allNums d = sortUnique $ dv >>= (\x -> ((genN (d `div` x) x)*) <$> (mkNums x))
  where
    dv = [x | x <- [1..(pred d)], d `mod` x == 0]
    sortUnique = fmap head . group . sort
    genN n d = sum $ take n $ ((10^) . (*d) <$> [0..])
    mkNums d = [10^(pred d)..(pred (10^d))]

procRange (a, b) = takeWhile (<=b) . dropWhile (<a) $ nums
  where
    nums = [numDigits a..numDigits b] >>= allNums

part1 = sum . (parse >=> genList)
part2 = sum . (parse >=> procRange)

main = getContents >>= print . (part1 &&& part2)
