# mstknnclust

**mstknnclust** implements the MST-kNN clustering algorithm, which
determines the number of clusters automatically by recursively
intersecting the Minimum Spanning Tree (MST) and *k*-Nearest Neighbor
(*k*NN) proximity graphs.

## Installation

``` r
# From CRAN
install.packages("mstknnclust")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("jorgeklz/package-mstknnclust")
```

## Usage

``` r
library(mstknnclust)

# Load the Indo-European languages dataset
data("dslanguages")

# Run clustering (only a distance matrix is needed)
results <- mst.knn(dslanguages)

# Results
results$cnumber   # number of clusters
results$partition # cluster assignments
results$network   # igraph object

# Plot
library(igraph)
plot(results$network,
     vertex.size  = 5,
     vertex.color = components(results$network)$membership,
     layout       = layout_with_fr(results$network, niter = 10000))
```

## Documentation

Full documentation and vignettes are available at the [pkgdown
site](https://jorgeklz.github.io/package-mstknnclust/).

## References

Inostroza-Ponta, M. (2008). *An Integrated and Scalable Approach Based
on Combinatorial Optimization Techniques for the Analysis of Microarray
Data*. Ph.D. thesis, University of Newcastle.

## License

GPL-2
