import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Array
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid

parse content = (elemIndices 'S' x, filter (not . null) $ elemIndices '^' <$> xs)
  where
    (x : xs) = lines content

split :: [Int] -> Int -> Writer (Sum Int) [Int]
split splitters beam
    | beam `elem` splitters = tell 1 $> [pred beam, succ beam]
    | otherwise = pure [beam]

part1 = getSum . execWriter . uncurry process
  where
    process start =
        foldl'
            (\beams splitters -> nub . concat <$> (beams >>= mapM (split splitters)))
            (pure start)

part2 :: ([Int], [[Int]]) -> Int
part2 (start, splitterList) = go (head start, 0)
  where
    go (i, j)
        | j >= depth = 1
        | hasSplitter i j = r ! (pred i, succ j) + r ! (succ i, succ j)
        | otherwise = r ! (i, succ j)

    r = listArray bounds [go (i, j) | (i, j) <- range bounds]
    bounds = ((0, 0), (width, depth))

    hasSplitter i j = j < length splitterList && i `elem` splitterList !! j

    depth = length splitterList
    width = succ . maximum $ concat splitterList

main = getContents >>= print . (part1 &&& part2) . parse
