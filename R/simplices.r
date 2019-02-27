#' @title Vertices and simplices af a simplicial complex object
#'
#' @description These functions return the vertices and the simplices of an
#'   object of class 'sc'.
#'
#' @details
#'
#' **sc** encodes simplicial complexes as bipartite [igraph::igraph] objects
#' prefixed with a new 'sc' class, whose nodes are partitioned into those
#' encoding the _vertices_ versus defining _simplices_ of the complex. The
#' simplices are taken to be these defining simplicies together with all of
#' their _faces_ (sub-simplices).
#'
#' The `"type"` attribute takes the values `FALSE` on the vertex nodes and
#' `TRUE` on the simplex nodes. The functions `sc_vertices()` and
#' `sc_simplices()` return the nodes of the 'sc' object corresponding to the
#' vertices and to the simplices, respectively. To obtain the vertices in a
#' specific set of simplices or the simplices containing a specific set of
#' vertices, pass the indices of these simplices or vertices to the
#' corresponding arguments.
#'
#' @name simplices
#' @import igraph
#' @param sc Bipartite [igraph::igraph] object, representing a simplicial
#'   complex.
#' @param vertices,simplices Numeric vertex IDs or character names of nodes of
#'   `sc` whose incident nodes are desired (if any). Defaults to `NULL`.
#' @param ... Integer, character, or 'igraph.vs' used to identify nodes of `sc`
#'   corresponding to vertices or simplices, as appropriate.
#' @example inst/examples/ex-wiki.r
NULL

#' @rdname simplices
#' @export
sc_vertices <- function(sc, simplices = NULL) {
  if (is.null(simplices)) return(V(sc)[!V(sc)$type])
  if (is.numeric(simplices) & any(V(sc)[simplices]$type == FALSE)) {
    simplices <- simplices + length(which(V(sc)$type == FALSE))
  }
  stopifnot(all(V(sc)[simplices]$type == TRUE))
  lapply(simplices, function(precluster) {
    V(sc)[setdiff(neighborhood(graph = sc, order = 1, nodes = precluster)[[1]],
                  V(sc)[precluster])]
  })
}

#' @rdname simplices
#' @export
sc_simplices <- function(sc, vertices = NULL) {
  if (is.null(vertices)) return(V(sc)[V(sc)$type])
  stopifnot(all(V(sc)[vertices]$type == FALSE))
  lapply(vertices, function(actor) {
    V(sc)[setdiff(neighborhood(graph = sc, order = 1, nodes = actor)[[1]],
                  V(sc)[actor])]
  })
}

#' @rdname simplices
#' @export
sc_faces <- sc_simplices

sc_containing_simplices <- function(sc, ...) {
  vertices <- unlist(list(...))
  vertices <- V(sc)[vertices]
  if (any(vertices$type)) stop("Some indices correspond to simplex nodes.")
  Reduce(intersect, neighborhood(sc, 1, vertices))
}

#' @rdname simplices
#' @export
sc_has_simplex <- function(sc, ...) {
  ss <- sc_containing_simplices(sc, ...)
  length(ss) > 0
}

#' @rdname simplices
#' @export
sc_simplex <- function(sc, ...) {
  vs <- V(sc)[unlist(list(...))]
  ss <- V(sc)[sc_containing_simplices(sc, vs)]
  if (length(ss) == 0) return(NULL)
  s <- ss[1]
  attr(s, "face") <- match(vs, neighborhood(sc, 1, s, mindist = 1)[[1]])
  s
}

#' @rdname simplices
#' @export
sc_intersection <- function(sc, ...) {
  simplices <- unlist(list(...))
  simplices <- V(sc)[simplices]
  if (any(! simplices$type)) stop("Some indices correspond to vertex nodes.")
  Reduce(intersect, neighborhood(sc, 1, simplices))
}

#' @rdname simplices
#' @export
sc_nearness <- function(sc, ...) {
  int <- sc_intersection(sc, ...)
  length(int) - 1
}
