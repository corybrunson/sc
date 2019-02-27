# a face list with some redundancies
x <- list(a = 1:4, b = 3:5, c = c(3,6), d = 7, e = c(3,6), f = c(3:5, 7))
# creating a simplicial complex removes duplicates
(sc <- sc_from_list(x))
plot(sc)
# further reduction can be done by testing for subset relations among the faces
(rsc <- sc_reduce(sc))
plot(rsc)
