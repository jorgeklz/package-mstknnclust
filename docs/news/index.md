# Changelog

## mstknnclust 1.0.0

- First major release.
- kNN construction uses a partial-sort approach that stores only
  `floor(log(n))` neighbours per node, yielding O(n^2 log n) time and
  O(n \* log(n)) working memory.
- MST is computed via
  [`igraph::mst()`](https://r.igraph.org/reference/mst.html) on a sparse
  edge list constructed from integer index vectors.
- All deprecated igraph function calls replaced with modern equivalents
  ([`components()`](https://r.igraph.org/reference/components.html),
  [`intersection()`](https://r.igraph.org/reference/intersection.html),
  [`is_connected()`](https://r.igraph.org/reference/components.html),
  etc.).
- Package structure reorganised following R Packages (2e) conventions:
  one file per exported function, testthat test suite, pkgdown site.
- Computational performance benchmarks added (see
  `inst/extdata/benchmark_performance.R`).
- Added pkgdown website at
  <https://jorgeklz.github.io/package-mstknnclust/>.

## mstknnclust 0.3.2

CRAN release: 2023-01-27

- Previous CRAN release.
