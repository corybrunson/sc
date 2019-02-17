# example simplicial complex from Wikimedia Commons
# https://commons.wikimedia.org/wiki/File:Simplicial_complex_example.png
wiki_sc <- sc_from_list(list(
  a = c(1,3,4),
  b = 1:2,
  c = 5,
  d = 6:9,
  e = c(6, 10:11),
  f = 10:12,
  g = c(11, 13:14),
  h = 14:15,
  i = c(14, 16),
  j = 15:16,
  k = c(13, 17),
  l = 17:18
))
print(wiki_sc)
plot(wiki_sc, vertex.label = NA)
