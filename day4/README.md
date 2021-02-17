# Problem

So the [problem](https://adventofcode.com/2020/day/4) is basically that we have a big text file full of records consiting of `key:value` pairs.
Each `key` is separated from its `value` by a `:`, pairs are separated by whitespace, and records (collections of key, value pairs) are separated by empty lines:

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

I'm finding it really tricky to get out of the imperative mindset!
In ruby I would probably do something like:

```
data = however you red the file to a string

# split the 

