module Main where

type Row = Int
type Column = Int
type Seat = (Row, Column)

seatID :: Seat -> Int
seatID (row, col) = row * 8 + col

parseSeat :: String -> Seat
parseSeat s = (row, col)
    where row = binaryPartition 0 127 $ stringToBinary 'F' rowString
          col = binaryPartition 0 7 $ stringToBinary 'L' colString
          (rowString, colString) = splitAt 7 s

stringToBinary :: Char -> String -> [Bool]
stringToBinary trueChar = map (==trueChar)

binaryPartition :: Int -> Int -> [Bool] -> Int
binaryPartition min max binary 
    | length binary == 1 = if head binary then min else max
    | length binary > 1 = if head binary
                     then binaryPartition min (midpoint min max) $ tail binary
                     else  binaryPartition (1 + midpoint min max) max $ tail binary

midpoint :: Int -> Int -> Int
midpoint a b = a + (b - a) `div` 2

main = do
    inputText <- readFile "boarding_passes.txt"
    let seatIDs = map (seatID . parseSeat) $ lines inputText
    print $ maximum seatIDs
