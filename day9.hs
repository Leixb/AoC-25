import Control.Arrow
import Data.Char
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP

type Coord = (Int, Int)
data Segment = V {x, y1, y2 :: Int} | H {y, x1, x2 :: Int} deriving (Show)

parse = fst . last . readP_to_S (((,) <$> (num <* char ',') <*> num) `endBy` char '\n')
 where
  num = read <$> munch1 isDigit :: ReadP Int

buildPairs l = sortOn (Down . size) [(x, y) | (x : xs) <- tails l, y <- xs]
size ((x1, y1), (x2, y2)) = (succ . abs $ x1 - x2) * (succ . abs $ y1 - y2)

part1 = fmap size . listToMaybe . snd

toSegment ((x1, y1), (x2, y2))
  | x1 == x2 = V{x = x1, y1 = min y1 y2, y2 = max y1 y2}
  | y1 == y2 = H{y = y1, x1 = min x1 x2, x2 = max x1 x2}
  | otherwise = error "Segment is not axis-aligned"

validBox :: [Segment] -> [Coord] -> Coord -> Coord -> Bool
validBox segments points (x1, y1) (x2, y2) =
  not $
    contains (min x1 x2, min y1 y2) (max x1 x2, max y1 y2) points
      || any
        ((`any` segments) . intersects . toSegment)
        [ ((x1, y1), (x2, y1))
        , ((x2, y1), (x2, y2))
        , ((x2, y2), (x1, y2))
        , ((x1, y2), (x1, y1))
        ]
 where
  contains :: Coord -> Coord -> [Coord] -> Bool
  contains a@(x1, y1) b@(x2, y2) =
    any
      ( \v@(x, y) ->
          inRange (a, b) v
            && not (x `elem` [x1, x2] && y `elem` [y1, y2]) -- allow corners
      )

  intersects (V{x, y1, y2}) (H{y, x1, x2}) = x1 < x && x < x2 && y1 < y && y < y2
  intersects h@(H{}) v@(V{}) = intersects v h
  intersects _ _ = False

part2 (points, pairs) = size <$> find (uncurry $ validBox segments points) pairs
 where
  segments = toSegment <$> zip points (h : t)
  Just (h, t) = uncons points

toSnd f a = (a, f a)

main = getContents >>= print . (part1 &&& part2) . toSnd buildPairs . parse
