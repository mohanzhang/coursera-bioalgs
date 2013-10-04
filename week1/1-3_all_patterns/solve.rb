# input: on the first line, a pattern p; on the second line a string s
# output: indices where p appears in s, separated by a space

p, s = IO.read("stepic_dataset.txt").split("\n")
is = []

# idea: walk entire string, recording the initial index of any occurrences of
# p, noting that occurrences of p can overlap each other

(0..(s.length-p.length)).each do |i|
  is << i if s.slice(i, p.length) == p
end

puts is.join(" ")
