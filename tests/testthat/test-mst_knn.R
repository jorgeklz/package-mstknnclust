test_that("mst.knn returns correct structure on small synthetic data", {
  set.seed(1987)
  n <- 50; m <- 10
  x <- matrix(runif(n * m, min = -5, max = 10), nrow = n, ncol = m)
  d <- as.matrix(dist(x))

  res <- mst.knn(d)

  expect_type(res, "list")
  expect_named(res, c("network", "cnumber", "cluster", "partition", "csize"))
  expect_s3_class(res$network, "igraph")
  expect_true(res$cnumber >= 1L)
  expect_equal(length(res$cluster), n)
  expect_equal(sum(res$csize), n)
  expect_equal(nrow(res$partition), n)
  expect_true(all(res$partition$cluster %in% seq_len(res$cnumber)))
})

test_that("mst.knn works on dslanguages dataset", {
  data("dslanguages", package = "mstknnclust")
  res <- mst.knn(dslanguages)

  expect_equal(res$cnumber, 17L)
  expect_equal(length(res$cluster), 84L)
  expect_equal(sum(res$csize), 84L)
})

test_that("mst.knn rejects non-square input", {
  expect_error(mst.knn(matrix(1:6, nrow = 2, ncol = 3)),
               "equal numbers of rows and columns")
})

test_that("mst.knn works with suggested.k", {
  set.seed(42)
  d <- as.matrix(dist(matrix(rnorm(200), nrow = 20)))
  res <- mst.knn(d, suggested.k = 2)

  expect_type(res, "list")
  expect_true(res$cnumber >= 1L)
})

test_that("mst.knn handles n = 2 edge case", {
  d <- matrix(c(0, 1, 1, 0), nrow = 2)
  rownames(d) <- colnames(d) <- c("a", "b")
  res <- mst.knn(d)

  expect_equal(res$cnumber, 1L)
  expect_equal(length(res$cluster), 2L)
})
