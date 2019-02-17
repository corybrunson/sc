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
#' `TRUE` on the simplex nodes. The functions `vertices()` and `simplices()`
#' return the nodes of the 'sc' object corresponding to the vertices and to the
#' simplices, respectively. To obtain the vertices in a specific set of
#' simplices or the simplices containing a specific set of vertices, pass the
#' indices of these simplices or vertices to the corresponding arguments.
#'
#' @name simplices
#' @import igraph
#' @param sc Bipartite [igraph::igraph] object, representing a simplicial
#'   complex.
#' @param vertices,simplices Numeric vertex IDs or character names of nodes of
#'   \code{sc} whose incident nodes are desired (if any). Defaults to
#'   \code{NULL}.
NULL

#' @rdname simplices
#' @export
vertices <- function(sc, simplices = NULL) {
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
simplices <- function(sc, vertices = NULL) {
  if (is.null(vertices)) return(V(sc)[V(sc)$type])
  stopifnot(all(V(sc)[vertices]$type == FALSE))
  lapply(vertices, function(actor) {
    V(sc)[setdiff(neighborhood(graph = sc, order = 1, nodes = actor)[[1]],
                  V(sc)[actor])]
  })
}
