# load data
data(wiki_sc)
# inspection and visualization
print(wiki_sc)
plot(wiki_sc, vertex.label = NA)
# vertices and simplices
sc_vertices(wiki_sc)
sc_simplices(wiki_sc)
sc_vertices(wiki_sc, simplices = letters[1:3])
sc_simplices(wiki_sc, vertices = 1:5)
