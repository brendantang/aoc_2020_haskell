# Day 4, part 2

Where part 1 of this problem just had me check if each passport had the seven required fields, part 2 wants me to validate each field in a different way.

Here are the rules:
> - byr (Birth Year) - four digits; at least 1920 and at most 2002.
> - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
> - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
> - hgt (Height) - a number followed by either cm or in:
>   - If cm, the number must be at least 150 and at most 193.
>   - If in, the number must be at least 59 and at most 76.
> - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
> - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
> - pid (Passport ID) - a nine-digit number, including leading zeroes.
> - cid (Country ID) - ignored, missing or not.

In my solution to part 1 I represented a `Passport` as a list of `(String,String)` for each key-value pair, like `("byr","1994")`.
One imperative approach to validating these passport records would be to loop over each key, value pair and use a `case` statement to apply different rules to the value based on the key.

And you could do something like that in Haskell:

```haskell
isValid :: (String,String) -> Bool
isValid (key, value) = 
    case key of "byr" -> validateBYR value
                "iyr" -> validateIYR value
                -- And so on...
                otherwise -> False
```

But I keep thinking about the saying ['Parse Don't Validate'](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/). 
(That blog post has a good explanation of why I should have been using a `Map` instead of a `[(String,String)]`—instead of checking whether there are duplicate keys (which I didn't do anyway, lol), it's more efficient and expressive to just represent it as a data type where duplicate keys are not possible.)

I had started to rewrite the `Passport` data type using record syntax, but aside from eliminating the duplicate keys issue (which happens not to mater with this data set), it's fairly awkward and doesn't feel very "Haskell."
Because either you can say every field in the record is a String:

```haskell
data Passport = Passport {
    byr :: String
  , iyr :: String
  , eyr :: String
  , hgt :: String
  , hcl :: String
  , ecl :: String
  , pid :: String
}
```

But then we're still validating, not parsing—each field is still just a string that we have to check against some validation function.
But think about the validation function I'll have to write for the `byr` field—it will have to take a string, and check if it can be converted into an integer between 1920 and 2002.
If we go through that work, why not just actually convert it to an integer?

I think the "Haskell way" to represent the Passport data type is actually more like this:

```haskell
data Passport = Passport BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID CountryID
```

Where `BirthYear`, `IssueYear`, and so on are each their own types:

```haskell
data Height = Inches Integer
            | Centimeters Integer

data EyeColor = Amb 
              | Blu
              | Brn 
              | Gry 
              | Grn 
              | Hzl 
              | Oth
```

With functions to parse a string into that type:

```haskell
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
```



