# =============================================================================
# benchmark_performance.R  (v3)
# Supplementary material for:
#   "mstknnclust: An R Package for Graph-Based Clustering via MST-kNN Intersection"
#   Software Impacts, Elsevier
#
# Measures:
#   - Wall-clock time via system.time() (median of multiple runs)
#   - Peak heap memory via gc(reset=TRUE) max-used Vcells
#
# Dependencies:
#   install.packages(c("mstknnclust", "dbscan", "mclust", "cluster"))
# =============================================================================

#library(mstknnclust)   # uncomment if using installed CRAN version
library(dbscan)
library(mclust)
library(cluster)

# ── Helper: synthetic distance matrix from a 5-cluster Gaussian mixture ───────
make_dist <- function(n, p = 10, k_true = 5, seed = 42) {
  set.seed(seed)
  centers <- matrix(rnorm(k_true * p, sd = 3), nrow = k_true)
  labels  <- sample(seq_len(k_true), n, replace = TRUE)
  X       <- centers[labels, ] + matrix(rnorm(n * p), nrow = n)
  as.matrix(dist(X))
}

# ── Helper: median wall-clock time over iters runs ────────────────────────────
median_time <- function(expr, iters = 3L, envir = parent.frame()) {
  times <- numeric(iters)
  for (i in seq_len(iters)) {
    gc(verbose = FALSE)
    times[i] <- system.time(eval(expr, envir = envir))["elapsed"]
  }
  median(times)
}

# ── Helper: memory delta (MB) via gc() ────────────────────────────────────────
# Measures the increase in peak R heap (MB) caused by running the expression,
# by subtracting the baseline heap measured right before the run.
# gc() in R 4.5 returns 7 columns; column 7 = max used (Mb).
mem_delta_mb <- function(expr, envir = parent.frame()) {
  invisible(gc(reset = TRUE, full = TRUE, verbose = FALSE))
  baseline <- sum(gc(reset = TRUE, verbose = FALSE)[, 7])
  eval(expr, envir = envir)
  peak <- sum(gc(verbose = FALSE)[, 7])
  round(max(0, peak - baseline), 1)
}

# ── Dataset sizes ─────────────────────────────────────────────────────────────
sizes <- c(100, 500, 1000, 2000, 5000)

# ── Run benchmarks ────────────────────────────────────────────────────────────
results <- lapply(sizes, function(n) {
  
  cat(sprintf("\n=== n = %d ===\n", n))
  D       <- make_dist(n)
  X_low   <- cmdscale(D, k = 2)
  iters   <- if (n <= 500) 3L else 1L
  eps_val <- quantile(D[lower.tri(D)], 0.05)
  
  e_mstknn <- quote(mst.knn(D))
  e_dbscan <- quote(dbscan::dbscan(D, eps = eps_val, minPts = 5))
  e_mclust <- quote(mclust::Mclust(X_low, verbose = FALSE))
  e_pam    <- quote(cluster::pam(D, k = 5, diss = TRUE))
  
  data.frame(
    n        = n,
    method   = c("mstknnclust", "dbscan", "mclust (BIC)", "PAM"),
    time_sec = c(median_time(e_mstknn, iters),
                 median_time(e_dbscan, iters),
                 median_time(e_mclust, iters),
                 median_time(e_pam,    iters)),
    mem_mb   = c(mem_delta_mb(e_mstknn),
                 mem_delta_mb(e_dbscan),
                 mem_delta_mb(e_mclust),
                 mem_delta_mb(e_pam))
  )
})

perf <- do.call(rbind, results)

# ── Print ─────────────────────────────────────────────────────────────────────
cat("\n\n=== Computational Performance Results ===\n\n")
perf_fmt          <- perf
perf_fmt$time_sec <- sprintf("%.3f s",  perf$time_sec)
perf_fmt$mem_mb   <- sprintf("%.1f MB", perf$mem_mb)
print(perf_fmt, row.names = FALSE)

# ── Save ──────────────────────────────────────────────────────────────────────
write.csv(perf, file = "benchmark_results.csv", row.names = FALSE)
cat("\nResults saved to benchmark_results.csv\n")

# ── Log-log time plot  ────────────────────────────────
library(ggplot2)
ggplot(perf, aes(x = n, y = time_sec, color = method, shape = method)) +
  geom_line() + geom_point(size = 3) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Wall-clock time vs. dataset size",
       x = "n", y = "Median time (s)") +
  theme_bw()
ggsave("benchmark_time_plot.pdf", width = 7, height = 4)