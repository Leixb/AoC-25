import Control.Arrow
import Data.Char
import Text.ParserCombinators.ReadP

import Data.Array qualified as A
import Data.Map.Strict qualified as M

parse = M.fromList . fst . last . readP_to_S (((,) <$> (munch1 isAlpha <* string ": ") <*> (munch1 isAlpha `sepBy` char ' ')) `endBy` char '\n')

out = 0 :: Int -- index of out node

buildAdjList m = (keys, adj)
  where
    keys = M.insert "out" out . snd . M.mapAccumWithKey (\a k _ -> (succ a, a)) (succ out) $ m
    adj = A.listArray (out, out + M.size m) $ [] : (fmap (keys M.!) <$> M.elems m)

findPaths adj src dest = go src
  where
    go i
      | i == dest = 1 :: Int
      | otherwise = sum $ (r A.!) <$> (adj A.! i)

    r = A.listArray bounds $ go <$> A.range bounds
    bounds = A.bounds adj

part1 (keys, adj) = findPaths adj (keys M.! "you") out

-- Since graph must be acyclic, one of fft_dac or dac_fft will be 0
part2 (keys, adj)
  | fft_dac /= 0 = svr_fft * fft_dac * dac_out
  | otherwise = svr_dac * dac_fft * fft_out
    where
      [svr, fft, dac] = (keys M.!) <$> ["svr", "fft", "dac"]
      svr_fft = findPaths adj svr fft
      fft_dac = findPaths adj fft dac
      dac_out = findPaths adj dac out

      svr_dac = findPaths adj svr dac
      dac_fft = findPaths adj dac fft
      fft_out = findPaths adj fft out

main = getContents >>= print . (part1 &&& part2) . buildAdjList . parse
