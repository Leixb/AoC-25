import Control.Monad
import Data.Char
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

main = getContents >>= print . sum . (parse >=> genList)
