import Data.List
import Data.Function
import Control.Arrow

parse = fmap (fmap (read . pure)) . lines

solve n = sum . fmap (sum . zipWith (*) (iterate (*10) 1) . reverse . go n)
  where
    go :: Int -> [Int] -> [Int]
    go 0 l = pure $ maximum l
    go n l = mx : go (n-1) (drop idx l)
      where
        -- use minimumBy since if there are multiple least elements, we want the leftmost one.
        (idx, mx) = minimumBy (compare `on` (negate . snd)) . zip [1..] . take (length l - n) $ l

main = getContents >>= print . (solve 1 &&& solve 11) . parse
