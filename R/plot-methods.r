#' @title Plotting simplicial complexes
#'
#' @description These functions use [igraph::igraph.plotting] functionality to
#'   plot a simplicial complex, i.e. a bipartite [igraph::igraph] object
#'   prefixed with class 'sc'.
#'
#' @name sc-plot
#' @importFrom viridis viridis magma plasma inferno
#' @param x Bipartite [igraph::igraph] object, representing a simplicial
#'   complex.
#' @param layout A [igraph::layout] function, or the result of such a function
#'   on the projection of the simplicial complex `x`.
#' @param simplex.colors Character; the color palette from which to assign
#'   colors to the simplices (by dimension). Matched to one of the **viridis**
#'   palettes (`"viridis"`, `"magma"`, `"plasma"`, `"inferno"`) or
#'   `"gray.colors"`.
#' @param simplex.alpha Numeric value between `0` and `1` indicating the
#'   transparency of the simplices.
#' @param ... Additional parameters passed to other [graphics::plot()] methods.
#' @example inst/examples/ex-wiki.r
NULL

#' @rdname sc-plot
#' @method plot sc
#' @export
plot.sc <- function(
  x,
  simplex.colors = "viridis", simplex.alpha = .5,
  layout = layout_with_fr, ...
) {
  
  args <- list(...)
  if (! is.null(args[["vertex.label"]])) {
    if (! is.na(args[["vertex.label"]])) {
      warning("'vertex.label' assignments may not match cluster indices.")
    }
  }
  proj_simplex_colors <- simplex_colors(
    x,
    simplex.colors = simplex.colors,
    simplex.alpha = simplex.alpha
  )
  
  # higher-dimensional simplices
  simps <- lapply(higher_simplices(x), as.integer)
  
  # 0- and 1-simplices
  proj <- bipartite_projection(x, which = "false", multiplicity = FALSE)
  V(proj)$frame.color <- "#ffffff00"
  E(proj)$color <- proj_simplex_colors[1]
  E(proj)$width <- 1.75
  # default aesthetics
  if (! "vertex.size" %in% names(args)) {
    V(proj)$size <- 8 + 5 / sqrt(vcount(proj)) * sqrt(degree(proj))
  }
  if (! "vertex.label.family" %in% names(args)) {
    V(proj)$label.family <- "sans"
  }
  if (! "vertex.label.color" %in% names(args)) {
    V(proj)$label.color <- "#000000bb"
  }
  if (! "vertex.label.cex" %in% names(args)) {
    V(proj)$label.cex <- .25 + 2.5 / sqrt(vcount(proj))
  }
  
  # generate layout if necessary
  if (is.function(layout)) {
    lay <- layout.norm(layout(proj))
  } else {
    lay <- layout
  }
  
  # set plotting window
  plot.igraph(
    proj, layout = lay,
    vertex.color = "#ffffff00", vertex.frame.color = "#ffffff00",
    edge.color = "#ffffff00", vertex.label = NA, edge.label = NA
  )
  # plot higher simplices
  for (simp in simps) {
    graphics::polygon(
      x = lay[simp[grDevices::chull(lay[simp, ])], ],
      col = proj_simplex_colors[length(simp) - 1],
      border = NA
    )
  }
  # plot 1-skeleton
  plot.igraph(proj, layout = lay, add = TRUE, ...)
  
  invisible(x)
}

# plot simplicial complex
higher_simplices <- function(sc) {
  stopifnot(is_bipartite(sc))
  stopifnot(! is.null(V(sc)$name))
  tab <- table(as_edgelist(sc, names = FALSE)[, 2])
  vids <- as.numeric(names(tab[tab > 2]))
  simps <- neighborhood(sc, 1, vids, mindist = 1)
  names(simps) <- V(sc)$name[vids]
  simps
}

# assign a default scheme to a simplicial complex
simplex_colors <- function(sc, simplex.colors, simplex.alpha = .5) {
  simplex.colors <- match.arg(tolower(simplex.colors), c(
    "gray.colors", "viridis", "magma", "plasma", "inferno"
  ))
  n <- max(degree(sc)[vertex_attr(sc, "type")]) - 1
  cols <- if (simplex.colors == "gray.colors") {
    grDevices::gray.colors(n = n, alpha = simplex.alpha)
  } else {
    viridis_fun <- get(simplex.colors)
    viridis_fun(n = n, alpha = simplex.alpha)
  }
  cols
}
