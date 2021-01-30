module Main where
import Data.List (tails, nub, sort)

main = do
  ledger <- getContents
  let numbers = map read $ words ledger :: [Int]
  let (d:duos) = duosWithSum 2020 numbers
  let (t:trios) = triosWithSum 2020 numbers
  let duoProduct = fst d * snd d
  let trioProduct = foldl (*) 1 t
  print $ "Answer part 1: " ++ (show duoProduct) ++ "    Answer part 2: " ++ (show trioProduct)

duosWithSum :: Int -> [Int] -> [(Int, Int)]
duosWithSum n xs = [(x,y) | (x:ys) <- tails (nub xs)
                            , y <- ys
                            , y + x == n]
                     

triosWithSum :: Int -> [Int] -> [[Int]]
triosWithSum n xs = [[x,y,z] | (x:ys) <- tails (nub xs)
                            , (y:zs) <- tails ys
                            , z <- zs
                            , y + x + z == n]
                     

