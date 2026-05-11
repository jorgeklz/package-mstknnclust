#' Generates the solution when only singletons are yielded
#'
#' @param total_nodos Total number of nodes in data matrix.
#' @param nodos_singletons Nodes list with cluster singletons.
#' @return An object of class "igraph" as a network representing the clustering solution.
#' @keywords internal
#' @export
only.single.graphs <- function(total_nodos, nodos_singletons) {

  clusteres_unidos <- igraph::graph(edges = NULL, n = NULL, directed = FALSE)
  clusteres_unidos <- clusteres_unidos + igraph::vertices(total_nodos)

  if (length(nodos_singletons) > 0) {
    single <- as.character(unlist(unique(nodos_singletons)))
    clusteres_unidos <- clusteres_unidos + igraph::path(single)
  }

  return(clusteres_unidos)
}
