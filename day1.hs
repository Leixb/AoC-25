module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Char
import Data.Functor
import Data.Functor.Identity
import Text.ParserCombinators.ReadP

n = 100
start = 50

parse = fst . last . readP_to_S (endBy rotation (char '\n'))
  where
    rotation = (*) <$> ((char 'L' $> (-1)) <++ (char 'R' $> 1)) <*> (read <$> munch isDigit)

part1 = length . filter (== 0) . fmap (`mod` n) . scanl (+) start

spins :: Int -> Int -> Writer [Int] Int
spins acc x = do
    when (abs x >= n) . tell . pure $ abs x `div` n -- full loops
    let res = acc + (x `rem` n)
        res' = res `mod` n

    when (res /= res') . tell . pure $ 1

    return res'

part2 = runIdentity . fmap sum . execWriterT . foldM spins start

main :: IO ()
main = getContents >>= (print . (part1 &&& part2) . parse)
