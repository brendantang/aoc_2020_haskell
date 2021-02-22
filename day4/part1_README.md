# Day 4, part 1

> I meant to jot down some notes on my thought process and ended up writing like a whole walkthrough/comparison of the solution in both Ruby and Haskell lol

So the [problem](https://adventofcode.com/2020/day/4) is basically that we have a big text file full of records consisting of `key:value` pairs.
Each `key` is separated from its `value` by a `:`, pairs are separated by whitespace, and records (collections of key, value pairs representing passports) are separated by empty lines:

```
byr:1983 iyr:2017
pid:796082981 cid:129 eyr:2030
ecl:oth hgt:182cm

iyr:2019
cid:314
eyr:2039 hcl:#cfa07d hgt:171cm ecl:#0180ce byr:2006 pid:8204115568

byr:1991 eyr:2022 hcl:#341e13 iyr:2016 pid:729933757 hgt:167cm ecl:gry

hcl:231d64 cid:124 ecl:gmt eyr:2039
hgt:189in
pid:#9c3ea1

...
```

Part 1 of the problem asks me to find the number of passports with all seven required fields ("cid" is not required).

I'm finding it really tricky to get out of the imperative mindset!

## Imperative (Ruby) solution

In ruby I would probably do something like:

```ruby
data = File.read("data.txt")

passport_hashes = data.split("\n\n").map do |passport_string|
  passport_string.split.inject({}) do |passport_hash, field_string|
    key, val = field_string.split(":")
    passport_hash[key] = val
    passport_hash
  end
end
```

Split the string on empty newlines getting a string of data for each passport, loop through each of those passport strings, split them on whitespace to get strings for each key:value pair, and loop through _those_ strings to construct a hash for the password.

Finally, to get the count of passports with all seven required fields present, I loop through the list of hashes, incrementing a counter for each passport with the required keys:

```ruby
RequiredKeys = %w[ byr iyr eyr hgt hcl ecl pid ]

number_of_passports_with_required_fields =
  passport_hashes.inject(0) do |count, passport|
    count += 1 if (RequiredKeys - passport.keys).empty?
  end

puts number_of_passports_with_required_fields
```

In other words, the imperative approach is to describe the steps to go from

```ruby
"byr:1983 iyr:2017
pid:796082981 cid:129 eyr:2030
ecl:oth hgt:182cm

iyr:2019
cid:314
eyr:2039 hcl:#cfa07d hgt:171cm ecl:#0180ce byr:2006 pid:8204115568"
```

to

```ruby
[
  "byr:1983 iyr:2017
  pid:796082981 cid:129 eyr:2030
  ecl:oth hgt:182cm",
  "iyr:2019
  cid:314
  eyr:2039 hcl:#cfa07d hgt:171cm ecl:#0180ce byr:2006 pid:8204115568"
]
```

to

```ruby
[
  ["byr:1983", "iyr:2017", "pid:796082981", "cid:129", "eyr:2030", "ecl:oth", "hgt:182cm"],
  ["iyr:2019", "cid:314", "eyr:2039", "hcl:#cfa07d", "hgt:171cm", "ecl:#0180ce", "byr:2006", "pid:8204115568"]
]
```

to

```ruby
[
  {"byr"=>"1983", "iyr"=>"2017", "pid"=>"796082981", "cid"=>"129", "eyr"=>"2030", "ecl"=>"oth", "hgt"=>"182cm"},
  {"iyr"=>"2019", "cid"=>"314", "eyr"=>"2039", "hcl"=>"#cfa07d", "hgt"=>"171cm", "ecl"=>"#0180ce", "byr"=>"2006", "pid"=>"8204115568"}
]
```

Where you can finally loop through each hash comparing its keys to the set of required keys.

I basically write a bunch of loops, each time pushing the data a little closer to a shape that's easier for me to work with.

## Haskell solution (declarative I guess?)

In some ways my Haskell solution feels like it's approaching the solution from the other end.

Where in the Ruby solution I started by saying what I have (the string read from `data.txt`) and described the steps to take to get what I want (the count of passports with all required fields),
in the Haskell solution I start by declaring what I want and working backwards from there.

```haskell
-- I start by saying "a passport is a list of fields, where each field is a pair of key, value."
type Passport = [PassportField]
type PassportField = (String, String)

main = do
    inputString <- readFile "data.txt"

    -- Then I say, "let's assume there's a function `parsePassports` which takes a string and turns it into a list of Passports.
    let passports = parsePassports inputString

    -- If I also have a function `hasRequiredFields` which tests if a Passport has all required fields,
    -- then the number of valid passports is the length of the list of Passports that pass that test.
    let passportCount = length $ filter hasRequiredFields passports 

    print $ show passportCount
```

I start by describing the solution we want (the count of valid passports) and breaking it into smaller solutions—a way to get a list of passports from a string, and a way to test if a passport is valid.

From there, I can continue "working my way backwards" by breaking those problems into smaller ones.
Let's start with testing whether a passport has all the required fields.

### Testing a Passport for required fields

```haskell
hasRequiredFields :: Passport -> Bool
hasRequiredFields passport = null (requiredKeys \\ keys passport)
```

`(\\)` is an operator that evaluates the difference between two lists, such that: `[1, 2, 3] \\ [2, 1]` is `[3]`.
So if the list of required keys `\\` the list of the passport's keys is empty, then we know the passport has all the required keys.

Now I've broken the `hasRequiredFields` test into two smaller problems: how to find the list of `requiredKeys`, and how to get the list of `keys` from a passport.

#### Getting required keys 

The list of required keys is just a list, so we can define it without breaking it down into any smaller problems:

```haskell
requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
```

#### Getting a list of keys from a Passport

```haskell
keys :: Passport -> [String]
keys passport = ks 
    where (ks, _) = unzip passport
```

A Passport is a list of string pairs (`[(String,String)]`) where the first item of each pair is the key and the other item is the value.
`unzip` is a function included in the Prelude which takes a list of pairs (`[(first thing, second thing)]`) and turns it into a pair of lists (`([all first things], [all second things])`).
So the `keys` function I wrote just says, "give me `ks`, where `ks` is the first list (the list of keys) from the pair of lists we get by unzipping passport, and throw the other one (the list of values) away (`_`)."

With complete definitions for `hasRequiredFields` and its subproblems `keys` and `requiredKeys`, we now have a way to `filter` a list of Passports to get only those with the required fields.

All that's left now is to get a list of passwords to filter!

### Parsing a string into a list of Passports

```haskell
parsePassports :: String -> [Passport]
parsePassports s = map parsePassport (splitOnEmptyLine s)
```

Again I'm optimistically starting at the solution and working my way backward.
To turn the input string from `data.txt` into a list of Passports, we have 2 subproblems:
- Split the string on empty newlines to get a string for each passport (`splitOnEmptyLine`)
- Parse a string representing a passport into a `Passport` (`parsePassport`).

If we know how to solve those two subproblems, then all we have to do is `map` over the list of passport strings we get from `splitOnEmptyLine`-ing `s`, and apply `parsePassport` to each of those strings.

### Parsing a string into a single passport

```haskell
parsePassport :: String -> Passport
parsePassport s = map parseField (words s)
```

Parsing a string into a `Passport` (which, remember, is a list of `PassportField`) breaks down to the subproblems of
- Splitting the string into a list of strings, one for each "key:value" clause
- Parsing a "key:value" string into a `PassportField` (a pair `(String,String)`).

The clauses for each field are delimited by a single space or newline character (`\n`), so the Prelude function `words` does exactly what we want: `words "hcl:#b6652a eyr:2028\nhgt:182cm"` evaluates to `["hcl:#b6652a", "eyr:2028", "hgt:182cm"]`.

If `parseField` can turn `"hgt:182cm"` into `("hgt", "182cm")`, then we just have to `map` over the list returned by `words` to get the list of `[("key","value"),("key2","value2")]` pairs that satisfies our definition of `Passport`.

#### Parsing a "key:value" string to a ("key","value") pair

```haskell
parseField :: String -> PassportField
parseField s = (key, value)
    where 
      (key,_:value) = break (==':') s
```

`break` is a Prelude function which splits a list (a String is just a list of Char) into a pair, where the first element is a list of elements from the start of the list up to the first element which does not satisfy some condition—in our case `==':'`.

So `break (==':') "mykey:myvalue"` evaluates to `("mykey", ":myvalue")`.
But we don't want that colon, so we assign `key` and `value` using pattern matching.

The `:` in `_:value` is a list operator that constructs a list by prepending a single element to another list.

So if I defined a list like this:
```haskell
myList = (1:[2,3,4])
```
Then `myList` would evaluate to `[1,2,3,4]`.

Pattern matching comes into play here because I'm using the `:` operator on the left side of my assignment expression. Example:

```haskell
(foo:bar) = ['h','e','l','l','o']
```
`foo` then evaluates to `'h'`, and `bar` is `['e','l','l','o']`

By the same token, when I assign `_:value = ":myvalue"`, I'm saying `_ = ':'` (which just throws it away), and `value = ['m','y','v','a','l','u','e']`


### Splitting the input string on empty newlines

The last thing left to solve is how to split the input string on empty newlines!
I was sort of surprised to have to write a function to do this.
Maybe I'm spoiled by Ruby, where you can pretty much always expect the standard library to have an expressive, built in method for common tasks. 
In ruby I could just call `split("\n\n")` on the string and get the list I want.

I was also thrown off by the different kinds of strings in Haskell.
The regular `String` type is just a synonym for `[Char]`, and I was surprised that the standard Prelude didn't have any built in function to, say, split a `String` into a list of `String` delimited by some arbitrary substring (like Ruby's `split`).

It took me a while to realize that the data type I would want to use is `Data.ByteString.Char8` if I want nice built in stuff like [`breakSubstring`](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString-Char8.html#g:16). 
But even that just performs the split once and returns a pair, so I would still have to write a function to apply `breakSubstring` recursively and build a list.

Just as good to stick with `String` and use the Prelude function `lines` instead:

```haskell
splitOnEmptyLine :: String -> [String]
splitOnEmptyLine string = firstElem : restOfTheElems
    where firstElem = unlines firstItemLines
          (firstItemLines, remainingLines) = break null (lines string)
          restOfTheElems
            | null remainingLines = []
            | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines

```

Coming from an imperative mindset, I'm finding it really hard to explain what's going on here but I'll try!
`splitOnEmptyLine` is basically saying:
- `splitOnEmptyLine string = firstElem : restOfTheElems` means give me a string `string`, and I'll give you the first item of the list... and also the rest of the list.
- `firstElem = unlines firstItemLines` - The first item of the list is the list of strings for the lines representing the first passport, collapsed back into a single string.
  ```haskell
  unlines ["pid:796082981 cid:129 eyr:2030","ecl:oth hgt:182cm"]
  -- evaluates to "pid:796082981 cid:129 eyr:2030\necl:oth hgt:182cm"
  ```
- `(firstItemLines, remainingLines) = break null (lines string)` - Here we see that `firstItemLines` we just used is the first item in the pair you get by `break`ing `(lines string)` using `null`.
  - `lines string` splits `string` into a list of strings, one for each line, so if `string` looks like this:
    ```
    "pid:796082981 cid:129 eyr:2030
    ecl:oth hgt:182cm

    iyr:2019

    cid:12"
    ```
    then `lines string` looks like: `["pid:796082981 cid:129 eyr:2030","ecl:oth hgt:182cm","","iyr:2019","","cid:12"]`
  - Notice empty newlines became empty strings, `""`.
  - `null` checks if a String (or any list) is empty, and `break null` splits a list into a pair of lists just before the first element for which `null` is true.
    ```haskell
    break null ["pid:796082981 cid:129 eyr:2030","ecl:oth hgt:182cm","","iyr:2019","","cid:12"]
    -- evaluates to (["pid:796082981 cid:129 eyr:2030","ecl:oth hgt:182cm"],["","iyr:2019","","cid:12"])
    ```
- So we know how to get the `firstElem` of our list, what about the `restOfTheElems?` Well...
  - `remainingLines` is the other list we got from `break null (lines string)` - it's a list of the lines after the lines for the first passport.
    - If we discard the leading `""` with `tail` and `unlines` it back into a String, then it looks just like our original input string, but shorter: `"iyr:2019\n\ncid:12"`.
    - `restOfTheElems` needs to be a list of strings, each representing a passport.
      And we have a function that takes a string in this format and parses it into just such a list—it's the function we're writing now!
- That means we can say `restOfTheElems = splitOnEmptyLine $ unlines $ tail remainingLines`
  - But eventually we'll parse through the whole input string, and there's nothing to keep our function from continuing to invoke itself, so it will just keep adding empty lists forever.
  - So we add a guard to handle the case where `remainingLines` is empty, meaning we've parsed all the input. That's how we end up with:
    ```haskell
    restOfTheElems
      | null remainingLines = []
      | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines
    ```


## Wrap-up

I'm sure my code could be more idiomatic, but I feel like the functional/declarative approach is starting to click for me in a way that it hadn't before working on this particular puzzle.

I really like the way Haskell encourages you to "start at the solution" and sort of work your way down the tree of subproblems.
I keep thinking it feels less like "solving problems" and more like "naming solutions."
Each solution you name is made up of some combination of other solutions, some of which already have names (like `length`, `filter`, `unzip`, `map` ...), others you can just name and deal with later (`hasRequiredFields`, `keys`...).

Some things I'm not crazy about / still need to adapt to:
- I get why there need to be different `String` types for the kinds of performance-intensive applications people use Haskell for, but for this kind of thing it felt like a pain.
- I missed having a really extensive, ergonomic standard library like in Go or Ruby. 
  I don't know if there was an easier way to `splitOnEmptyLine` than the function I wrote, but that's kind of the point. 
  For a task as common as "split a string on some substring," should a newcomer to the language have to deal with even the tiny friction of deciding whether to look for an external library or write their own function?
  Shouldn't there just be a built in `splitOnSubstring` function, for `String` AND `ByteString`?
- I think the language almost encourages too much brevity. 
  I felt like I had to exercise restraint to keep my code wordy enough that I could walk away for ten minutes and come back and have it mean anything to me.
  Part of that, of course, is because I'm new to the language.
  Also, most of the Haskell code I'm reading is in libraries where the problems being addressed are abstract enough that `x` or `xs` really is the most descriptive variable name, and I'm probably tempted to be too brief by mimicking their style.
  But I do think there are things about the language itself that nudge you in this direction:
  
  The way you're encouraged to break big, specific problems into smaller and more general subproblems is a strength, to be sure, but it also introduces a balancing act.
  Writing a bunch of increasingly abstract subproblems means you end up with a bunch of functions you can reuse in other situations, but you have to make sure you're making abstractions that strike the right balance of usefulness in other contexts and expressing your intent in the current context.

  Like I could have written `splitOnEmptyLine` in a way that would be more tightly coupled to the current use case:
  ```haskell
  getPassportStrings :: String -> [String]
  getPassportStrings string = firstPassport : restOfThePassports
      where firstPassport = unlines firstPassportLines
            (firstPassportLines, remainingLines) = break null (lines string)
            restOfThePassports
              | null remainingLines = []
              | otherwise = splitOnEmptyLine $ unlines $ tail remainingLines
  ```
  In a way this is more readable in the context that I'm working on, but it obscures the fact that I could use this function anywhere I want to split a string on empty lines!
  On the other hand, I could have written a more abstract function like `splitOnSubstring`, which could be used in more situations, but would also introduce a whole other idea to the program, and maybe distract the reader from the problem at hand.

  - One final quibble with the language design: I don't like that there are so many unpronouncable operators. How am I supposed to refer to `\\` or the `(x:xs)` list syntax in conversation?

All things considered I must be enjoying Haskell a lot, or I wouldn't have spent so much time writing these notes 🧐

