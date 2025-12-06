import Control.Arrow
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

op "*" = product
op "+" = sum

part1 s = sum $ zipWith ($) (op <$> a) (transpose $ fmap read <$> as)
  where
    (a : as) = reverse . fmap words . lines $ s

parseGroups = fst . last . readP_to_S (sepBy (endBy int eol) eol) . filter (/= ' ')
  where
    eol = char '\n'
    int = read <$> munch1 isDigit :: ReadP Int

part2 s = sum $ zipWith ($) (op <$> words a) (parseGroups . unlines $ reverse <$> transpose as)
  where
    (a : as) = reverse $ lines s

main = getContents >>= print . (part1 &&& part2)
