import Control.Arrow
import Data.Char
import Text.ParserCombinators.ReadP

import Data.Array qualified as A
import Data.Map.Strict qualified as M

parse = M.fromList . fst . last . readP_to_S (((,) <$> (munch1 isAlpha <* string ": ") <*> (munch1 isAlpha `sepBy` char ' ')) `endBy` char '\n')

findPaths src dest m = go (keys M.! src)
  where
    go i
      | i == target = 1
      | otherwise = sum $ (r A.!) <$> (adjList A.! i)

    r = A.listArray bounds $ go <$> A.range bounds

    target = keys M.! dest
    bounds = (0, pred $ M.size keys)

    keys = M.insert "out" 0 . snd . M.mapAccumWithKey (\a k _ -> (succ a, a)) 1 $ m
    adjList = A.listArray bounds $ [] : (fmap (keys M.!) <$> M.elems m)

part1 = findPaths "you" "out"

part2 m = svr_fft * fft_dac * dac_out + svr_dac * dac_fft * fft_out
  where
    svr_fft = findPaths "svr" "fft" m
    svr_dac = findPaths "svr" "dac" m

    dac_fft = findPaths "dac" "fft" m
    fft_dac = findPaths "fft" "dac" m

    fft_out = findPaths "fft" "out" m
    dac_out = findPaths "dac" "out" m

main = getContents >>= print . (part1 &&& part2) . parse
