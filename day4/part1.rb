data = File.read("data.txt")

passport_hashes = data.split("\n\n").map do |passport_string|
  passport_string.split.inject({}) do |passport_hash, field_string|
    key, val = field_string.split(":")
    passport_hash[key] = val
    passport_hash
  end
end

puts passport_hashes

RequiredKeys = %w[ byr iyr eyr hgt hcl ecl pid ]

number_of_passports_with_required_fields =
  passport_hashes.inject(0) do |n, passport|
    n += 1 if (RequiredKeys - passport.keys).empty?
    n
  end

puts number_of_passports_with_required_fields
