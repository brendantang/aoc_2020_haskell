module Main where
import Data.Ratio

type TreePattern = [Bool] 
type Mountain = [(Int, TreePattern)]
type Slope = (Int, Int)

strToTreePat :: String -> TreePattern
strToTreePat = map (=='#')

strToMtn :: String -> Mountain
strToMtn s = zip [0..] $ map strToTreePat $ lines s

hitTree :: Int -> TreePattern -> Bool
hitTree pos pat 
    | pos > length pat = False
    | otherwise = pat !! pos

treeHits :: Slope -> Mountain -> [Bool] 
treeHits slope = map hitTreeAtRow 
    where hitTreeAtRow (i, pat) = hitTree (tobogganPosition slope i pat) pat

countTreeHits :: Slope -> Mountain -> Int
countTreeHits slope mtn = length $ filter (==True) (treeHits slope mtn)

showStep :: Int -> TreePattern -> String
showStep position pat = map (showSquare position) indexedPat
    where indexedPat = zip [0..] pat
          showSquare :: Int -> (Int, Bool) -> Char
          showSquare tobogganPos (index, treePresence) 
              | tobogganPos == index && treePresence = 'X'
              | tobogganPos == index && not treePresence = 'O'
              | tobogganPos /= index && treePresence = '#' 
              | tobogganPos /= index && not treePresence = '.' 

showJourney :: Slope -> Mountain -> String
showJourney (dx, dy) mtn = unlines steps
    where steps = [ showStep (tobogganPosition (dx, dy) i pat) pat | (i, pat) <- mtn] 
          
tobogganPosition :: Slope -> Int -> TreePattern -> Int
tobogganPosition slope index pat 
    | isInt (position slope index) = mod (numerator (position slope index)) (length pat)
    | otherwise = length pat + 1 -- If you're in between squares, just hide your position off the screen

position :: Slope -> Int -> Ratio Int
position (dx, dy) i = i * dx % dy

isInt :: Ratio Int -> Bool
isInt x = denominator x == 1

main = do
    rawMap <- readFile "map.txt"
    let mtn = strToMtn rawMap
    let hits = countTreeHits (3, 1) mtn 
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1, 2)]
    let allHits = map (`countTreeHits` mtn) slopes
    
    putStr $ showJourney (1,2) mtn ++ "\nAnswer to part one: " ++ show hits ++ "\nAnswer to part 2: " ++ show (product allHits)
