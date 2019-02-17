#' @title Creating simplicial complexes
#'
#' @description These functions create 'sc' objects from various input sources.
#'
#' @name create
#' @param d A data frame of vertex-face membership with columns `vertex` and
#'   `simplex`.
#' @param l A list of simplices consisting of integer member vertex IDs.
#' @param sc A simplicial complex (an [igraph::igraph] object prefixed with
#'   class 'sc').
#' @param vertices A character or integer vector of vertex IDs, possibly
#'   including more than those included in `d` or `l`.
#' @param reduce Whether to iteratively remove simplices that are faces of other
#'   simplices.
#' @return A simplicial complex encoded as an [igraph::igraph] object prefixed
#'   with class 'sc'.
#' @example inst/examples/ex-redundancy.r
NULL

#' @rdname create
#' @export
sc_from_data_frame <- function(d, vertices = NULL, reduce = FALSE) {
  # incorporate any additional vertices
  d_verts <- as.character(unique(d$vertex))
  d_simps <- as.character(unique(d$simplex))
  if (is.null(vertices)) {
    vertices <- d_verts
    simplices <- d_simps
  } else {
    if (! all(d_verts %in% vertices)) {
      warning("Not all vertices in `d` appear in `vertices`.")
      new_vertices <- unique(setdiff(vertices, d_verts))
      vertices <- c(d_verts, new_vertices)
      new_simplices <- new_names(length(new_vertices), c(vertices, d_simps))
      simplices <- c(d_simps, new_simplices)
      d <- rbind(d, data.frame(vertex = new_vertices, simplex = new_simplices))
    }
  }
  # data frame of nodes
  nodes <- data.frame(
    name = c(vertices, simplices),
    type = rep(c(FALSE, TRUE), c(length(vertices), length(simplices)))
  )
  g <- graph_from_data_frame(d, directed = FALSE, vertices = nodes)
  class(g) <- c("sc", class(g))
  if (reduce) g <- reduce_sc(g)
  g
}

#' @rdname create
#' @export
sc_from_list <- function(l, vertices = NULL, reduce = FALSE) {
  x <- uniquify_list(l)
  if (is.null(names(x))) {
    names(x) <- new_names(length(x), c(names(x), unlist(x), vertices))
  }
  new_vertices <- setdiff(vertices, unlist(x))
  n <- length(new_vertices)
  if (n > 0) {
    new_vertices <- as.list(new_vertices)
    names(new_vertices) <- new_names(n, c(names(x), unlist(x), vertices))
    x <- c(x, new_vertices)
  }
  
  d <- data.frame(
    vertex = unname(unlist(x)),
    simplex = rep(names(x), sapply(x, length))
  )
  sc_from_data_frame(d, reduce = reduce)
}

uniquify_list <- function(x) {
  duped <- duplicated(x)
  x <- x[! duped]
  lapply(x, sort)
}

new_names <- function(n, avoid = NULL) {
  n_dots <- if (! is.null(avoid)) {
    max(regexpr("[^\\.]", avoid))
  } else 0
  paste0(paste(rep(".", n_dots), collapse = ""), 1:n)
}

#' @rdname create
#' @export
reduce_sc <- function(sc) {
  # set nodes
  s <- which(vertex_attr(sc, "type"))
  d <- degree(sc, s)
  # remove structurally equivalent simplex nodes
  # from highest- to lowest-dimension, remove faces of maximal simplices
  m <- max(d)
  if (m == 1) return(sc)
  # highest-dimensional simplices will be kept
  w <- s[which(d == m)]
  for (k in (m - 1):1) {
    # potential lower-dimensional simplices of higher-dimensional simplices
    f <- s[which(d == k)]
    subs <- sapply(neighborhood(sc, 1, f, mindist = 1), function(x) {
      sapply(neighborhood(sc, 1, w, mindist = 1), function(y) {
        all(x %in% y)
      })
    })
    # exclude redundancies
    i <- f[which(! apply(subs, 2, any))]
    w <- c(w, i)
  }
  # keep only irredundant maximal simplices
  sc <- delete_vertices(sc, sort(setdiff(s, w)))
  class(sc) <- c("sc", class(sc))
  sc
}
