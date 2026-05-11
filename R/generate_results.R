#' Generates clustering results
#'
#' @param g_clusters igraph object with all clusters as connected components.
#' @param distance.matrix The original distance matrix.
#' @return A list with cnumber, cluster, partition, csize, network.
#' @keywords internal
#' @export
generate.results <- function(g_clusters, distance.matrix) {

  final_grupos              <- igraph::components(g_clusters)
  final_grupos_membresia    <- final_grupos$membership
  final_grupos_num_clusters <- final_grupos$no
  final_grupos_cardinalidad <- final_grupos$csize

  vector_particion <- unname(
    final_grupos_membresia[
      sort(as.integer(names(final_grupos_membresia)), index.return = TRUE)$ix
    ]
  )
  names(vector_particion) <- rownames(distance.matrix)

  tabla_particion <- data.frame(object  = rownames(distance.matrix),
                                cluster = vector_particion,
                                stringsAsFactors = FALSE)
  tabla_particion <- tabla_particion[order(tabla_particion$cluster), ]
  rownames(tabla_particion) <- seq_len(nrow(tabla_particion))

  elementos_orden <- as.integer(names(final_grupos_membresia))
  g_clusters <- igraph::set_vertex_attr(g_clusters, "name",
                                        value = elementos_orden)

  return(list(network   = g_clusters,
              cnumber   = final_grupos_num_clusters,
              cluster   = vector_particion,
              partition = tabla_particion,
              csize     = final_grupos_cardinalidad))
}
