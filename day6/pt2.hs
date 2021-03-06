module Main where
import Data.List (intersect)

-- Copy-pasted from day 4
splitOnEmptyLine :: String -> [String]
splitOnEmptyLine s = firstElem : restOfTheElems
    where firstElem = unlines firstItemLines
          (firstItemLines, remainingLines) = break null (lines s)
          restOfTheElems
            | null remainingLines = []
            | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines

main = do
    inputText <- readFile "data.txt"
    let groupAnswers = map lines $ splitOnEmptyLine inputText
    let unanimousGroupAnswers = map (foldl intersect ['a'..'z']) groupAnswers
    print unanimousGroupAnswers 
    print $ "Sum of unanimous group answers:" ++ show (sum $ map length unanimousGroupAnswers)
