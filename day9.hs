import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

parse = fst . last . readP_to_S (endBy ((,) <$> (num <* char ',') <*> num) (char '\n'))
 where
  num = read <$> munch1 isDigit :: ReadP Int

allPairs l = [(x, y) | (x : xs) <- tails l, y <- xs]

size ((x1, y1), (x2, y2)) = (succ . abs $ x1 - x2) * (succ . abs $ y1 - y2)

main = getContents >>= print . maximum . fmap size . allPairs . parse
