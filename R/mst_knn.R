#' Performs the MST-kNN clustering algorithm
#'
#' @description Performs the MST-kNN clustering algorithm which generates a
#'   clustering solution with automatic number-of-clusters determination by
#'   recursively intersecting the Minimum Spanning Tree (MST) and the
#'   \emph{k}-Nearest Neighbor (\emph{k}NN) graphs.
#'
#' @param distance.matrix A numeric matrix or data.frame with equal numbers of
#'   rows and columns representing pairwise distances between objects.
#' @param suggested.k Optional. A numeric value representing the suggested
#'   number of nearest neighbours.
#' @return A list with elements \code{cnumber}, \code{cluster},
#'   \code{partition}, \code{csize}, \code{network}.
#' @author Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @examples
#'
#' set.seed(1987)
#' n <- 100; m <- 15
#' x <- matrix(runif(n * m, min = -5, max = 10), nrow = n, ncol = m)
#' d <- base::as.matrix(stats::dist(x, method = "euclidean"))
#' library("mstknnclust")
#' results <- mst.knn(d)
#' library("igraph")
#' plot(results$network,
#'      vertex.size  = 8,
#'      vertex.color = igraph::components(results$network)$membership,
#'      layout       = igraph::layout_with_fr(results$network, niter = 10000),
#'      main         = paste("MST-kNN  |  clusters =", results$cnumber))
#'
#' @export
mst.knn <- function(distance.matrix, suggested.k) {

  if (missing(distance.matrix))
    stop("You should specify a distance object of class matrix or dataframe.")

  if (nrow(distance.matrix) != ncol(distance.matrix))
    stop("distance.matrix must have equal numbers of rows and columns.")

  distance.matrix <- as.data.frame(distance.matrix)

  sk <- if (missing(suggested.k)) NULL else as.integer(suggested.k)

  # ── Initial intersection on the full node set ─────────────────────────────
  subgraphs <- seq_len(nrow(distance.matrix))
  inicio    <- .generate.intersections.fast(subgraphs, distance.matrix, sk)

  nodos_singletons  <- Filter(function(x) length(x) <= 1L, inicio$subgraphs)
  subgraphs         <- Filter(function(x) length(x) >= 2L, inicio$subgraphs)
  cluster_subgraphs <- list()
  x <- 1L

  if (inicio$cc == 1L && length(igraph::V(inicio$ccgraph)) > 1L) {
    cluster_subgraphs[[x]] <- inicio$ccgraph
  }

  # ── Recursive step ────────────────────────────────────────────────────────
  cc <- nrow(distance.matrix)

  while (cc > 1L) {

    acumulados <- list()

    if (length(subgraphs) > 0L) {
      for (y in seq_along(subgraphs)) {

        componente <- .generate.intersections.fast(subgraphs[[y]], distance.matrix, sk)

        nodos_singletons <- c(nodos_singletons,
                              Filter(function(z) length(z) <= 1L, componente$subgraphs))

        if (componente$cc == 1L) {
          if (length(igraph::V(componente$ccgraph)) > 1L) {
            cluster_subgraphs[[x]] <- componente$ccgraph
            x <- x + 1L
          }
        } else {
          nodos_tres_mas <- Filter(function(z) length(z) >= 2L, componente$subgraphs)
          if (length(nodos_tres_mas) > 0L) {
            acumulados <- c(acumulados, nodos_tres_mas)
          } else {
            nodos_singletons <- c(nodos_singletons,
                                  Filter(function(z) length(z) <= 1L, componente$subgraphs))
          }
        }
      }
    }

    subgraphs <- Filter(function(z) length(z) >= 2L, acumulados)
    if (length(subgraphs) < 1L) cc <- 1L
  }

  # ── Union of all cluster subgraphs ────────────────────────────────────────
  if (length(cluster_subgraphs) > 0L) {
    aristas_clusteres <- do.call(rbind,
                                 lapply(cluster_subgraphs, igraph::as_edgelist))
    aristas_clusteres <- unique(aristas_clusteres)
    if (!is.matrix(aristas_clusteres))
      aristas_clusteres <- matrix(aristas_clusteres, ncol = 2)
    clusteres_unidos  <- igraph::graph_from_data_frame(
      d = as.data.frame(aristas_clusteres[, 1:2, drop = FALSE]), directed = FALSE)
  } else {
    cat("\n Only singletons are yielded \n")
    clusteres_unidos <- only.single.graphs(seq_len(nrow(distance.matrix)),
                                           nodos_singletons)
  }

  # ── Generate results ──────────────────────────────────────────────────────
  if (length(igraph::V(clusteres_unidos)$name) == nrow(distance.matrix)) {
    resultados <- generate.results(clusteres_unidos, distance.matrix)
  } else {
    cat("\n Only", length(igraph::V(clusteres_unidos)$name),
        "nodes in solution. Clustering solution only will have these nodes. \n")
    nodos.presentes <- sort(as.numeric(igraph::V(clusteres_unidos)$name))
    resultados      <- generate.results(clusteres_unidos,
                                        distance.matrix[nodos.presentes, nodos.presentes])
  }

  return(resultados)
}
