import Control.Arrow
import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

parse = fst . last . readP_to_S ((,) <$> (shape `endBy` eol) <*> (region `endBy` eol))
  where
    shape = num *> char ':' *> eol *> (many1 ((char '#' $> 1) <++ (char '.' $> 0)) `endBy` eol)
    region = (,) <$> ((,) <$> (num <* char 'x') <*> num <* char ':' <* char ' ') <*> (num `sepBy` char ' ')

    eol = char '\n'
    num = read <$> munch1 isDigit :: ReadP Int

part1 shapes = length . filter (uncurry (>=) . (uncurry (*) *** sum . zipWith (*) (sum . concat <$> shapes)))

main = getContents >>= print . uncurry part1 . parse
