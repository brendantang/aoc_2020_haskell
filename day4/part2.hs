module Main where
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, catMaybes)
import Text.Read (readMaybe)


main = do
    inputString <- readFile "data.txt"
    print $ stringToPassports inputString



data Passport = Passport BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID  deriving (Show)

type BirthYear = Int
type IssueYear = Int
type ExpirationYear = Int
data Height = Inches Int
            | Centimeters Int
            deriving (Show)
type HairColor = String
data EyeColor = Amber  
              | Blue
              | Brown  
              | Gray   
              | Green  
              | Hazel  
              | Other
            deriving (Show)
type PassportID = String



-- Parsing a string to a list of passports


stringToPassports :: String -> [Maybe Passport]
stringToPassports s = map stringToPassport $ splitOnEmptyLine s

stringToPassport :: String -> Maybe Passport
stringToPassport s = keyValueListToPassport (stringToKeyValueList s)

keyValueListToPassport :: [(String,String)] -> Maybe Passport
keyValueListToPassport fields 
    | hasDuplicateKeys fields = Nothing
    | otherwise = maybePassport
        where parseField :: String -> (String -> Maybe a) -> Maybe a
              parseField key parser = parser $ fromMaybe "" $ lookup key fields
              maybePassport = do
                  byr <- parseField "byr" stringToBirthYear
                  iyr <- parseField "iyr" stringToIssueYear
                  eyr <- parseField "eyr" stringToExpirationYear 
                  hgt <- parseField "hgt" stringToHeight 
                  hcl <- parseField "hcl" stringToHairColor 
                  ecl <- parseField "ecl" stringToEyeColor 
                  pid <- parseField "pid" stringToPassportID
                  return $ Passport byr iyr eyr hgt hcl ecl pid



-- Working with key, value lists


splitOnEmptyLine :: String -> [String]
splitOnEmptyLine s = firstElem : restOfTheElems
    where firstElem = unlines firstItemLines
          (firstItemLines, remainingLines) = break null (lines s)
          restOfTheElems
            | null remainingLines = []
            | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines

stringToKeyValueList :: String -> [(String,String)]
stringToKeyValueList s = map stringToKeyValuePair $ words s

stringToKeyValuePair :: String -> (String,String)
stringToKeyValuePair s = (key, value)
    where 
      (key,_:value) = break (==':') s

hasDuplicateKeys :: [(String, String)] -> Bool
hasDuplicateKeys pairs = length (keys pairs) /= length setKeys
    where setKeys = Set.fromList $ keys pairs

keys :: [(String, String)] -> [String]
keys = map fst



-- Parsing strings to passport field data types


stringToEyeColor :: String -> Maybe EyeColor
stringToEyeColor s = 
    case s of "amb" -> Just Amber
              "blu" -> Just Blue
              "brn" -> Just Brown 
              "gry" -> Just Gray 
              "grn" -> Just Green
              "hzl" -> Just Hazel
              "oth" -> Just Other
              _ -> Nothing


-- Three ways to go from String -> maybe a year in a range, verbose to concise

stringToBirthYear :: String -> Maybe BirthYear
stringToBirthYear s 
    | isJust year = yearInRange 1920 2002 $ fromJust year
    | otherwise = Nothing
    where year = readMaybe s :: Maybe Int

stringToIssueYear :: String -> Maybe IssueYear
stringToIssueYear s = maybe Nothing (yearInRange 2010 2020) (readMaybe s :: Maybe Int)
    
stringToExpirationYear  :: String -> Maybe ExpirationYear
stringToExpirationYear s = yearInRange 2020 2030 =<< (readMaybe s :: Maybe Int)

yearInRange :: Int -> Int -> Int -> Maybe Int
yearInRange min max year 
    | year >= min && year <= max = Just year 
    | otherwise = Nothing


stringToHeight :: String -> Maybe Height
stringToHeight s 
    | length s > 5 = Just $ Centimeters 100
    | otherwise = Just $ Centimeters 100

stringToHairColor :: String -> Maybe HairColor
stringToHairColor s
    | length s > 5 = Just "col"
    | otherwise = Just "hair col"

stringToPassportID :: String -> Maybe PassportID
stringToPassportID s
    | length s > 5 = Just "col"
    | otherwise = Just "Passport id"
