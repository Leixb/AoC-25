import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

op "*" = product
op "+" = sum

part1 s = sum . getZipList $ ZipList (op <$> a) <*> ZipList (transpose $ fmap read <$> as)
  where
    (a : as) = reverse . fmap words . lines $ s

part2 s = sum . getZipList $ ZipList (op <$> words a) <*> ZipList (parseGroups . unlines $ reverse <$> transpose as)
  where
    (a : as) = reverse $ lines s
    parseGroups = fst . last . readP_to_S (sepBy (endBy int eol) eol) . filter (/= ' ')
    eol = char '\n'
    int = read <$> munch1 isDigit :: ReadP Int

main = getContents >>= print . (part1 &&& part2)
