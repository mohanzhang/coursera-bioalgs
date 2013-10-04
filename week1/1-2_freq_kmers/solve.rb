# takes as input a string t and integer k, assumed to be on separate lines
# returns as output all the most frequent k-mers in t

t, k = IO.read("stepic_dataset.txt").split("\n")

# idea: only substrings of length 11 can be 11-mers, so just go through
# linearly 11-mer by 11-mer and keep tabs on the count. This is O(n) and must
# be the most efficient way since you have to look at the whole string at least
# once.

last_i = t.size - 11
counts = Hash.new

(0..last_i).each do |i|
  substring = t[i..(i+10)]
  if counts.has_key?(substring)
    counts[substring] += 1
  else
    counts[substring] = 1
  end
end

max_occurrences = counts.values.max
most_frequent = []

counts.each_pair do |k,v|
  most_frequent << k if v == max_occurrences
end

puts most_frequent.join(" ")
