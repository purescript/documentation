while (l = gets()) do
  out = l.dup
  while (m = out.match(/\[\[(.*?)\]\]/))
    out.sub!(m[0],"[" + m[1] + "](" + m[1].gsub(" ", "-") + ".md)")
  end
  puts out
end
