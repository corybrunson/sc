# load data
data(wiki_sc)
# inspection and visualization
print(wiki_sc)
plot(wiki_sc, vertex.label = NA)
# vertices and simplices
vertices(wiki_sc)
simplices(wiki_sc)
vertices(wiki_sc, simplices = letters[1:3])
simplices(wiki_sc, vertices = 1:5)
