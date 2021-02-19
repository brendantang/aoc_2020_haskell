module Main where
import Data.List ((\\))

type Passport = [PassportField]
type PassportField = (String, String)

main = do
    inputString <- readFile "data.txt"
    let passports = parsePassports inputString
    print passports
    let passportCount = length $ filter hasRequiredFields passports 
    print $ show passportCount



-- Testing a passport for required keys

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport = null $ requiredKeys \\ keys passport

requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

keys :: Passport -> [String]
keys passport = ks 
    where (ks, _) = unzip passport



-- Parsing a list of Passports from a string

parsePassports :: String -> [Passport]
parsePassports s = map parsePassport (splitOnEmptyLine s)

parsePassport :: String -> Passport
parsePassport s = map parseField (words s)

parseField :: String -> PassportField
parseField s = (key, value)
    where 
      (key,_:value) = break (==':') s

splitOnEmptyLine :: String -> [String]
splitOnEmptyLine s = firstElem : restOfTheElems
    where firstElem = unlines firstItemLines
          (firstItemLines, remainingLines) = break null (lines s)
          restOfTheElems
            | null remainingLines = []
            | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines

