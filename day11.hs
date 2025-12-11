import Control.Arrow
import Data.Char
import Text.ParserCombinators.ReadP

import Data.Array qualified as A
import Data.Map.Strict qualified as M

parse = M.fromList . fst . last . readP_to_S (((,) <$> (munch1 isAlpha <* string ": ") <*> (munch1 isAlpha `sepBy` char ' ')) `endBy` char '\n')

-- convert Strings to increasing integers
toInts :: M.Map String a -> M.Map String Int
toInts = M.insert "out" 0 . snd . M.mapAccumWithKey (\a k _ -> (succ a, a)) 1

part1 m = go you
  where
    go 0 = 1
    go i = sum $ (r A.!) <$> (adjList A.! i)

    keys = toInts m
    you = keys M.! "you"

    bounds = (0, pred $ M.size keys)
    r = A.listArray bounds $ go <$> A.range bounds

    adjList = A.listArray bounds $ [] : (fmap (keys M.!) <$> M.elems m)

main = getContents >>= print . part1 . parse
