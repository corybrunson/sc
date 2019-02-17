#' @title Printing simplicial complexes
#'
#' @description These functions adapt [tibble::format.tbl()],
#'   [tibble::print.tbl()], and the unexported function
#'   `node_tibble()` from **[tidygraph::tidygraph]** to conveniently display data separately on the
#'   observation and cluster nodes of the bipartite [igraph::igraph] object that
#'   encodes a simplicial complex.
#'
#' @name sc-print
#' @param x Bipartite [igraph::igraph] object, representing a simplicial
#'   complex.
#' @param ... Additional parameters passed to [tibble::trunc_mat()].
#' @example inst/examples/ex-wiki.r
NULL

#' @rdname sc-print
#' @method print sc
#' @export
print.sc <- function(x, ...) {
  tbl <- tibble::as_tibble(c(
    list(.vid = as.integer(V(x))),
    igraph::vertex_attr(x)
  ))
  arg_list <- list(...)
  # vertex data
  v_tbl <- tbl[tbl$type == FALSE, -match("type", names(tbl)), drop = FALSE]
  for (y in names(v_tbl)) if (!some_defined(v_tbl[[y]])) v_tbl[[y]] <- NULL
  v_trunc <- do.call(
    tibble::trunc_mat,
    utils::modifyList(arg_list, list(x = v_tbl, n = 5))
  )
  names(v_trunc$summary)[1] <- "Vertex data"
  # simplex data
  f_tbl <- tbl[tbl$type == TRUE, -match("type", names(tbl)), drop = FALSE]
  for (y in names(f_tbl)) if (!some_defined(f_tbl[[y]])) f_tbl[[y]] <- NULL
  f_trunc <- do.call(
    tibble::trunc_mat,
    utils::modifyList(arg_list, list(x = f_tbl, n = 5))
  )
  names(f_trunc$summary)[1] <- "Face data"
  cat(
    "# A simplicial complex of ", nrow(f_tbl),
    " faces on ", nrow(v_tbl),
    " vertices\n",
    sep = ""
  )
  cat("#\n")
  print(v_trunc)
  cat("#\n")
  print(f_trunc)
  invisible(x)
}

some_defined <- function(x) any(is.na(x) == FALSE)
