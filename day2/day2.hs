module Main where 

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

sledValid :: Password -> Bool
sledValid p = (charCount <= maxOfChar p) && (charCount >= minOfChar p)
  where 
        charCount = length $ filter (==requiredChar p) $ value p

tobogganValid :: Password -> Bool
tobogganValid p = (x == a && y /= a) || (x /= a && y == a)
    where
        x = value p !! (minOfChar p - 1)
        y = value p !! (maxOfChar p - 1)
        a = requiredChar p

main = do
  t <- getContents
  let passwords = map parseLine (lines t)
  let sledValidCount = length $ filter sledValid passwords
  let tobogganValidCount = length $ filter tobogganValid passwords
  print $ "sled valid: " ++ show sledValidCount ++ "toboggan valid: " ++ show tobogganValidCount
