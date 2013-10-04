t = IO.read("stepic_dataset.txt")

comp = {"A" => "T", "C" => "G"}
comp.merge!(comp.invert)

# Ok, this problem is ridiculous. No-brainer one-liner:
puts (t.scan(/./).map {|c| comp[c]}.reverse.join(""))
