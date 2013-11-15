# Was too lazy to try to profile and optimize haskell solution, so resigned to
# using iterative strategy in ruby.

s = IO.read("E-coli.txt")
k = 9
l = 500
t = 3

# We use the same lemma here: any k-mer that has an (L,t)-clump is also the
# start of an (L,t)-clump

clumps = []

=begin
(0..(s.length - l - 1)).each do |i|
  l_segment = s[i..(i+l)]
  kmer = l_segment[0..(k-1)]
  clumps << kmer if l_segment.scan(kmer).length >= t
end
=end

i = 0

while i < (s.length - l) do
  l_segment = s[i..(i+l)]
  kmer = l_segment[0..(k-1)]
  if l_segment.scan(kmer).length >= t
    clumps << kmer
  end
  i += 1
end


puts clumps.size

clumps.uniq!

puts clumps.size
