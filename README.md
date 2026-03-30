# mstknnclust

**mstknnclust** is an R package implementing the MST-kNN graph-based clustering algorithm. It partitions a dataset by computing the intersection of the Minimum Spanning Tree (MST) and the *k*-Nearest Neighbor (kNN) graph. The value of *k* is determined automatically from the data, so no hyperparameters need to be specified by the user.

The package is available on CRAN:  
[![CRAN version](https://www.r-pkg.org/badges/version/mstknnclust)](https://CRAN.R-project.org/package=mstknnclust)

---

## Algorithm overview

Given a pairwise distance matrix for *n* objects, the algorithm proceeds as follows:

1. Build a **complete graph** where edge costs equal the pairwise distances.
2. Compute the **MST** via Prim's algorithm.
3. Compute the **kNN graph**, where *k* = min(floor(ln(*n*)), smallest *k* that makes the kNN graph connected).
4. Retain only edges present in **both** graphs (intersection).
5. If more than one connected component results, apply steps 1-4 recursively to each component until every component has exactly one connected component.
6. Take the union of all component results as the final partition.

The result is a parameter-free partition that adapts to the geometry of the data.

---

## Installation

Install the stable release from CRAN:

```r
install.packages("mstknnclust")
```

Install the development version from this repository:

```r
# install.packages("devtools")
devtools::install_github("jorgeklz/package-mstknnclust")
```

### Dependencies

- **R** >= 3.2.5  
- **Imports:** `igraph`, `stats`  
- **Suggests:** `knitr`, `rmarkdown`

---

## Quick start

```r
library(mstknnclust)

# Load the bundled Indo-European languages distance matrix
data(dslanguages)

# Run MST-kNN clustering (no parameters required)
results <- mst.knn(dslanguages)

# Inspect results
results$cnumber   # number of clusters found
results$cluster   # cluster assignment per object
results$csize     # size of each cluster

# Visualize the clustering network
library(igraph)
plot(
  results$network,
  vertex.size  = 8,
  vertex.color = igraph::clusters(results$network)$membership,
  layout       = igraph::layout.fruchterman.reingold(results$network, niter = 10000),
  main         = paste("MST-kNN  |  Clusters =", results$cnumber)
)
```

---

## Main functions

| Function | Description |
|---|---|
| `mst.knn(d)` | Main entry point. Runs the full MST-kNN algorithm on distance matrix `d`. Returns cluster assignments, partition matrix, cluster sizes, and an `igraph` network object. |
| `generate.complete.graph(nodes, d)` | Builds the complete weighted graph from a node list and distance matrix. |
| `generate.mst(edges)` | Computes the Minimum Spanning Tree from a complete-graph edge list. |
| `generate.knn(edges, nodes)` | Computes the kNN graph with automatic *k* selection. |
| `generate.intersections.mst.knn(mst, knn)` | Intersects the MST and kNN graphs. |
| `compute.costs.proximity.graph(graph, edges)` | Attaches edge costs to a proximity graph. |
| `generate.results(network)` | Extracts the cluster summary from the final `igraph` object. |

Full documentation is available via `help(package = "mstknnclust")` and on [CRAN](https://CRAN.R-project.org/package=mstknnclust).

---

## Bundled datasets

| Dataset | Description |
|---|---|
| `dslanguages` | Lexical cognacy distance matrix for 84 Indo-European languages (84 x 84). |
| `dsyeastexpression` | Gene expression distance matrix for *S. cerevisiae* microarray data. |

---

## Validation and applications

The algorithm has been applied in peer-reviewed publications across three domains:

- **Computational linguistics:** recovery of Indo-European language family structure from lexical cognacy distances.
- **Genomics:** co-expression module detection in *S. cerevisiae* microarray data and GPU-accelerated gene clustering.
- **Seismology:** geographical clustering of aftershock events following the M_W 7.8 Ecuador earthquake.

---

## Citation

If you use this package, please cite:

```
Parraga-Alava J, Inostroza-Ponta M, Moscato P (2026).
mstknnclust: An R Package for Graph-Based Clustering via MST-kNN Intersection.
Software Impacts. https://doi.org/10.1016/j.simpa.2025.xxxxxx
```

To get the citation from within R:

```r
citation("mstknnclust")
```

---

## Contributing

Contributions, bug reports, and feature requests are welcome.

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature/your-feature`.
3. Commit your changes following conventional commit style.
4. Open a pull request describing the motivation and the changes made.

Please ensure that new code passes `R CMD check` with no errors or warnings before submitting a pull request.

---

## License

This package is distributed under the **GPL-2** license. See the [LICENSE](LICENSE) file for details.

---

## Contact

Jorge Parraga-Alava  
Facultad de Ciencias Informaticas, Universidad Tecnica de Manabi, Ecuador  
Email: jorge.parraga@utm.edu.ec  
ORCID: [0000-0001-8558-9122](https://orcid.org/0000-0001-8558-9122)
