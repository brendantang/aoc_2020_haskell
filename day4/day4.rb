data = "byr:1983 iyr:2017
pid:796082981 cid:129 eyr:2030
ecl:oth hgt:182cm

iyr:2019
cid:314
eyr:2039 hcl:#cfa07d hgt:171cm ecl:#0180ce byr:2006 pid:8204115568

byr:1991 eyr:2022 hcl:#341e13 iyr:2016 pid:729933757 hgt:167cm ecl:gry

hcl:231d64 cid:124 ecl:gmt eyr:2039
hgt:189in
pid:#9c3ea1
"

# Split into an array on empty lines
entries = data.split("\n\n").map do |str|
  puts str.split
  str.split.inject({}) do |record, key_val_string|
    key, val = key_val_string.split(":")
    record[key] = val
  end
end

print entries
