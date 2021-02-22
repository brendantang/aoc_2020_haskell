module Main where
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, catMaybes)
import Text.Read (readMaybe)


main = do
    inputString <- readFile "data.txt"
    let passports = catMaybes $ stringToPassports inputString
    print $ length passports
    -- mapM_ print passports
    -- let invalidPassportStrings = filter (isNothing . stringToPassport) (splitOnEmptyLine inputString)
    -- mapM_ print invalidPassportStrings
    -- print $ length invalidPassportStrings
  
  
  
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
    | isJust year = numberInRange 1920 2002 $ fromJust year
    | otherwise = Nothing
    where year = readMaybe s :: Maybe Int

stringToIssueYear :: String -> Maybe IssueYear
-- stringToIssueYear s = maybe Nothing (numberInRange 2010 2020) (readMaybe s :: Maybe Int)
stringToIssueYear s = numberInRange 2010 2020 =<< (readMaybe s :: Maybe Int)
    
stringToExpirationYear  :: String -> Maybe ExpirationYear
stringToExpirationYear s = numberInRange 2020 2030 =<< (readMaybe s :: Maybe Int)

numberInRange :: Int -> Int -> Int -> Maybe Int
numberInRange min max n
    | n >= min && n <= max = Just n
    | otherwise = Nothing


stringToHeight :: String -> Maybe Height
stringToHeight s
    | null unit || null number = Nothing
    -- "cm" and "in" using `fmap` two different ways to pass a Maybe Int to the Height constructor to return a Maybe Height.
    | unit == "cm" = fmap Centimeters $ maybe Nothing (numberInRange 150 193) (readMaybe number :: Maybe Int)
    | unit == "in" = fmap Inches (numberInRange 59 76 =<< (readMaybe number :: Maybe Int))
    | otherwise = Nothing
    where (number, unit) = span (\ c -> c `elem` ['0'..'9']) s

stringToHairColor :: String -> Maybe HairColor
stringToHairColor ('#':hex) 
    | length hex == 6 && all (\c -> c `elem` (['0'..'9'] ++ ['a'..'f'])) hex = Just ('#':hex)
    | otherwise = Nothing
stringToHairColor _ = Nothing

stringToPassportID :: String -> Maybe PassportID
stringToPassportID s
    | length s == 9 && all (\c -> c `elem` ['0'..'9']) s = Just s
    | otherwise = Nothing
