# Find all occurrences of CTTGATCAT in the Vibrio cholerae genome

s = IO.read("Vibrio_cholerae.txt")
p = "CTTGATCAT"
is = []

(0..(s.length-p.length)).each do |i|
  is << i if s.slice(i, p.length) == p
end

puts is.join(" ")
# I thought it would be massive but it wasn't
#File.open("output.txt", 'w') {|f| f.write(is.join(" "))}
