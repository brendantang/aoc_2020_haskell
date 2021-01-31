module Main where
import Debug.Trace

data Password = Password { requiredChar :: Char 
                         , minOfChar :: Int
                         , maxOfChar :: Int
                         , value :: String
                         } deriving (Show)

parseLine :: String -> Password
parseLine s = Password {
  requiredChar = rc
  , minOfChar = read charMin :: Int
  , maxOfChar = read charMax :: Int
  , value = pw
}
  where
       (charMin, s') = span (/='-') s -- "1-4 f: abbdf"
       (charMax, s'') = span (/=' ') $ dropWhile (== '-') s' -- "-4 f: abbdf"
       (rc:s''') = tail s'' -- "f: abbdf"
       pw = tail $ dropWhile (/=' ') s''' -- ": abbdf" 

passwordValid :: Password -> Bool
passwordValid p = (charCount <= maxOfChar p) && (charCount >= minOfChar p)
  where 
        charCount = length $ filter (==requiredChar p) $ value p

main = do
  t <- getContents
  let passwords = map parseLine (lines t)
  let validCount = length $ filter (passwordValid) passwords
  print validCount
