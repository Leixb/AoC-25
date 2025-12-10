import Data.Bits
import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

type Machine = ([Int], [[Int]], [Int])

parse :: String -> [Machine]
parse = fst . last . readP_to_S (line `endBy` char '\n')
  where
    line = (,,) <$> (light <* space) <*> (wiring <* space) <*> joltage

    light = between (char '[') (char ']') $ many $ (char '#' $> 1) <++ (char '.' $> 0)
    wiring = between (char '(') (char ')') numList `sepBy` space
    joltage = between (char '{') (char '}') numList

    numList = (read <$> munch1 isDigit) `sepBy` char ',' :: ReadP [Int]
    space = char ' '

part1 :: Machine -> Int
part1 (l, w, _) = go [light] 1
  where
    light = foldr1 (\b acc -> shift acc 1 .|. b) l
    wiring = foldl' setBit 0 <$> w
    go values depth = if 0 `elem` step then depth else go step $ succ depth
      where
        step = values >>= \v -> xor v <$> wiring

main = getContents >>= print . sum . fmap part1 . parse
