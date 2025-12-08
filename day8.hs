import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP

import Data.Array.Unboxed qualified as A
import Data.Map.Strict qualified as M

parse = fst . last . readP_to_S (endBy (sepBy (read <$> munch1 isDigit) (char ',')) (char '\n'))

sortedPairs l = sortOn dist [(x, y) | (x : ys) <- tails l, y <- ys]
  where
    dist = uncurry $ (sum .) . zipWith (\a b -> (b - a) ^ 2)

merge l = scanl' f (initialAssocs, initialSizes)
  where
    f s@(assocs, sizes) (a, b) = case compare ia ib of
        GT -> f s (b, a)
        LT ->
            ( M.map (\x -> if x == ib then ia else x) assocs
            , sizes A.// [(ib, 0), (ia, (sizes A.! ia) + (sizes A.! ib))]
            )
        EQ -> s
      where
        (ia, ib) = (assocs M.! a, assocs M.! b)

    initialAssocs = M.fromList $ zip l [1 ..]
    initialSizes = A.listArray (1, length l) $ repeat 1 :: A.UArray Int Int

main = do
    contents <- parse <$> getContents
    let pairs = sortedPairs contents
        merged = merge contents pairs
        n = findIndex ((== length contents) . (A.! 1) . snd) merged

    print $ product . take 3 . sortBy (comparing Down) . A.elems . snd <$> merged !? 1000
    print $ uncurry (*) . (head *** head) . (pairs !!) . pred <$> n
