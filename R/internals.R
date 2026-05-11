# ─────────────────────────────────────────────────────────────────────────────
# INTERNAL: sparse kNN  —  O(n^2 log n) time, O(n * k_natural) working memory
# ─────────────────────────────────────────────────────────────────────────────
#
# Only the k_natural = floor(log(n)) nearest neighbours per row are ever
# computed and stored.  The connectivity loop is bounded to k_cols iterations
# so it never accesses columns outside nbr_order.
#
.generate.knn.fast <- function(distance.matrix, nodes.list, suggested.k = NULL) {

  dist_sub  <- as.matrix(distance.matrix)[nodes.list, nodes.list, drop = FALSE]
  n         <- length(nodes.list)

  # For n == 2 both nodes are mutual nearest neighbours; short-circuit.
  if (n == 2L) {
    g_knn <- igraph::make_graph(c(1L, 2L), n = 2L, directed = FALSE)
    igraph::V(g_knn)$name <- as.character(nodes.list)
    edges_df <- data.frame(object_i = nodes.list[1], object_j = nodes.list[2],
                           d_ij = dist_sub[1, 2], stringsAsFactors = FALSE)
    return(list(edges.knn.graph = edges_df, k = 1L, knn.graph = g_knn))
  }

  k_natural <- max(1L, as.integer(floor(log(n))))

  diag(dist_sub) <- Inf

  # Partial sort: only keep the k_natural nearest neighbours per row.
  # k_cols >= 1 is guaranteed because n >= 3 here.
  k_cols <- max(1L, min(k_natural, n - 1L))

  # When k_cols == 1, apply() returns a plain vector (one scalar per row),
  # and t() of a vector gives a 1 x n matrix instead of n x 1.
  # We avoid t(apply(...)) entirely and use a safer construction:
  if (k_cols == 1L) {
    nbr_order <- matrix(apply(dist_sub, 1, which.min), nrow = n, ncol = 1L)
  } else {
    nbr_order <- t(apply(dist_sub, 1, function(row) order(row)[seq_len(k_cols)]))
  }

  # Build union-kNN edge vectors for a given k (must be <= k_cols)
  .knn_edges <- function(k) {
    nb_mat   <- nbr_order[, seq_len(k), drop = FALSE]   # n x k local indices
    from_all <- rep(seq_len(n), times = k)
    to_all   <- as.integer(nb_mat)
    # Forward direction (from < to)
    keep_fwd <- from_all < to_all
    # Reverse direction (to < from, i.e. neighbour points back)
    keep_rev <- to_all < from_all
    af <- c(from_all[keep_fwd], to_all[keep_rev])
    at <- c(to_all[keep_fwd],   from_all[keep_rev])
    ud <- !duplicated(af * (n + 1L) + at)
    list(from = as.integer(af[ud]), to = as.integer(at[ud]))
  }

  # Find minimum k in 1..k_cols that connects the graph
  k_conectado <- k_cols
  for (k in seq_len(k_cols)) {
    e      <- .knn_edges(k)
    if (length(e$from) == 0L) next
    edges_vec <- as.integer(as.vector(rbind(e$from, e$to)))
    g_test <- igraph::make_graph(edges_vec, n = n, directed = FALSE)
    igraph::V(g_test)$name <- as.character(nodes.list)
    if (igraph::is_connected(g_test)) {
      k_conectado <- k
      break
    }
  }

  valor_k <- min(k_natural, k_conectado)
  if (!is.null(suggested.k)) valor_k <- min(valor_k, as.integer(suggested.k))
  valor_k <- max(1L, valor_k)

  # Final kNN graph
  e_final <- .knn_edges(valor_k)
  if (length(e_final$from) == 0L) {
    g_knn <- igraph::make_empty_graph(n = n, directed = FALSE)
    igraph::V(g_knn)$name <- as.character(nodes.list)
  } else {
    edges_vec <- as.integer(as.vector(rbind(e_final$from, e_final$to)))
    g_knn <- igraph::make_graph(edges_vec, n = n, directed = FALSE)
    igraph::V(g_knn)$name <- as.character(nodes.list)
    g_knn <- igraph::simplify(g_knn, remove.loops = TRUE, remove.multiple = FALSE)
  }

  # Edge data.frame
  el <- igraph::as_edgelist(g_knn, names = FALSE)
  if (nrow(el) == 0L) {
    edges_df <- data.frame(object_i = integer(0), object_j = integer(0),
                           d_ij = numeric(0))
  } else {
    ni       <- nodes.list[el[, 1]]
    nj       <- nodes.list[el[, 2]]
    d_ij     <- dist_sub[el]
    edges_df <- data.frame(object_i = ni, object_j = nj, d_ij = d_ij,
                           stringsAsFactors = FALSE)
  }

  return(list(edges.knn.graph = edges_df, k = valor_k, knn.graph = g_knn))
}


# ─────────────────────────────────────────────────────────────────────────────
# INTERNAL: MST via igraph::mst() on a sparse edge list
# ─────────────────────────────────────────────────────────────────────────────
#
# Builds the complete graph from the upper triangle of dist_sub as an edge
# list (n*(n-1)/2 rows), which is unavoidable for an exact MST on a dense
# distance matrix, but avoids constructing a dense n x n igraph object.
#
.generate.mst.fast <- function(distance.matrix, nodes.list) {

  dist_sub <- as.matrix(distance.matrix)[nodes.list, nodes.list, drop = FALSE]
  n        <- length(nodes.list)

  # Trivial case: 2 nodes, 1 edge
  if (n == 2L) {
    el_char <- matrix(as.character(nodes.list), nrow = 1, ncol = 2)
    gmst_i  <- igraph::graph_from_edgelist(el_char, directed = FALSE)
    igraph::E(gmst_i)$weight <- dist_sub[1, 2]
    tabla <- data.frame(object_i = nodes.list[1], object_j = nodes.list[2],
                        d_ij = dist_sub[1, 2], stringsAsFactors = FALSE)
    return(list(edges.mst.graph = tabla, mst.graph = gmst_i))
  }

  # Upper triangle indices (vectorised)
  rows <- rep(seq_len(n - 1L), times = (n - 1L):1L)
  cols <- unlist(lapply(seq_len(n - 1L), function(i) seq.int(i + 1L, n)))
  w    <- dist_sub[cbind(rows, cols)]

  g_complete <- igraph::graph_from_edgelist(
    cbind(as.character(nodes.list[rows]),
          as.character(nodes.list[cols])),
    directed = FALSE
  )
  igraph::E(g_complete)$weight <- w

  gmst_i <- igraph::mst(g_complete, algorithm = "prim")

  tabla           <- as.data.frame(igraph::as_edgelist(gmst_i, names = TRUE),
                                   stringsAsFactors = FALSE)
  colnames(tabla) <- c("object_i", "object_j")
  tabla$object_i  <- as.numeric(tabla$object_i)
  tabla$object_j  <- as.numeric(tabla$object_j)
  tabla$d_ij      <- igraph::E(gmst_i)$weight

  return(list(edges.mst.graph = tabla, mst.graph = gmst_i))
}


# ─────────────────────────────────────────────────────────────────────────────
# INTERNAL: MST-kNN intersection for one subgraph
# ─────────────────────────────────────────────────────────────────────────────
.generate.intersections.fast <- function(nodes.list, distance.matrix, suggested.k = NULL) {

  nodes.list <- base::sort(nodes.list)

  if (length(nodes.list) <= 1L)
    stop("Error: only one object (node) in subgraph.")

  mst_res <- .generate.mst.fast(distance.matrix, nodes.list)
  knn_res <- .generate.knn.fast(distance.matrix, nodes.list, suggested.k)

  # igraph::intersection() requires both graphs to have the same vertex set.
  # Ensure both graphs contain all nodes.list vertices (some may be isolated).
  node_names <- as.character(nodes.list)
  mst_g <- mst_res$mst.graph
  knn_g <- knn_res$knn.graph

  missing_mst <- node_names[!node_names %in% igraph::V(mst_g)$name]
  if (length(missing_mst) > 0L)
    mst_g <- mst_g + igraph::vertices(missing_mst)

  missing_knn <- node_names[!node_names %in% igraph::V(knn_g)$name]
  if (length(missing_knn) > 0L)
    knn_g <- knn_g + igraph::vertices(missing_knn)

  inter_graph <- igraph::intersection(mst_g, knn_g, keep.all.vertices = TRUE)

  cc      <- igraph::components(inter_graph)
  sub_gfs <- lapply(seq_len(cc$no),
                    function(x) as.numeric(igraph::V(inter_graph)$name[cc$membership == x]))

  return(list(subgraphs = sub_gfs, ccgraph = inter_graph, cc = cc$no))
}
