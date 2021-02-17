module Main where
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.List ((\\))
import Data.Maybe (mapMaybe, catMaybes)
import Text.Read (readMaybe)


type PassportField = (String,String)
type Validity = Bool

data Passport = Passport {
    byr :: Maybe Int
  , iyr :: Maybe Int
  , eyr :: Maybe Int
  , hgt :: Maybe String 
  , hcl :: Maybe String
  , ecl :: Maybe String
  , pid :: Maybe String
  , cid :: Maybe String
} deriving (Show)

parsePassports :: BS.ByteString -> [Passport]
parsePassports s = mapMaybe (passportFromFields . parseFields . BS.unwords) (splitOnEmpty (BS.lines s))

-- Returns a passport with all required keys
passportFromFields :: [PassportField] -> Maybe Passport
passportFromFields fields 
    | requireKeys fields = Just Passport{
        byr = readFieldWith parseBYR "byr" fields
      , iyr = readFieldWith parseBYR "byr" fields
      , eyr = readFieldWith parseBYR "byr" fields
      , hgt = readFieldWith parseHGT "hgt" fields
      , hcl = readFieldWith parseHGT "hgt" fields
      , ecl = readFieldWith parseHGT "hgt" fields
      , pid = readFieldWith parseHGT "hgt" fields
      , cid = readFieldWith parseHGT "hgt" fields
    }    
    | otherwise = Nothing
        where parseBYR s = readMaybe s :: Maybe Int
              parseHGT s = readMaybe s :: Maybe String

readFieldWith :: (String -> Maybe a) -> String -> [PassportField] -> Maybe a
readFieldWith f key fields = maybe Nothing f (lookup key fields)
    -- case lookup key fields of
         -- Nothing -> Nothing
         -- Just value -> f value

parseFields :: BS.ByteString -> [PassportField]
parseFields s = map parseField $ BS.words s

parseField :: BS.ByteString -> PassportField
parseField s = (BS.unpack key, BS.unpack value)
    where 
      (key:rest) = BS.split ':' s
      (value:_) = rest

-- Take a list of ByteString and group it into a list of lists, delimited by empty bytestrings.
-- Example: ["foo", "", "bar", "baz"] -> [["foo"],["bar, "baz"]]
splitOnEmpty :: [BS.ByteString] -> [[BS.ByteString]]
splitOnEmpty xs = firstEntry:otherEntries
    where (firstEntry, rest) = break BS.null xs
          otherEntries 
              | null rest = []
              | otherwise = splitOnEmpty $ tail rest

requireKeys :: [PassportField] -> Bool
requireKeys fields = null $ requiredKeys \\ keys fields 

keys :: [PassportField] -> [String]
keys passport = k
    where (k,_) = unzip passport

requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

isValid :: Passport -> Bool
isValid passport = False

main = do
    inputHandle <- openFile "data.txt" ReadMode
    inputText <- BS.hGetContents inputHandle
    let passportsWithRequiredFields = parsePassports inputText
    print $ "entries with required keys: " ++ show (length passportsWithRequiredFields)
