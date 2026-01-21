# ============================================================
# Multi-objective benchmarking
# MO-Jaya vs NSGA-II on ZDT1/2/3 + DTLZ1/2
#
# Key design choices (to make this robust for a paper revision):
#  1) HV computed on the raw objective set F (no ND filtering needed).
#  2) IGD implemented manually (stable + version-independent).
#  3) MO-Jaya objectives are recomputed from decision vectors *only if*
#     the returned Pareto_Front contains x1..xn_var (otherwise MO-Jaya
#     is marked as NA and the script continues).
#
# Outputs:
#   - results_summary.csv
#   - pareto_plot.png  (ZDT1)
#
# Requires: Jaya, mco, emoa, ggplot2
# ============================================================

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(Jaya)
  library(mco)
  library(emoa)
  library(ggplot2)
})

# ----------------------------
# Helper: bounds
# ----------------------------
make_bounds <- function(n_var, lower_val = 0, upper_val = 1) {
  list(lower = rep(lower_val, n_var), upper = rep(upper_val, n_var))
}

# ----------------------------
# Benchmark functions (minimize): return length-2 numeric
# ----------------------------
zdt1 <- function(x) {
  f1 <- x[1]
  g  <- 1 + 9 * sum(x[-1]) / (length(x) - 1)
  f2 <- g * (1 - sqrt(f1 / g))
  c(f1, f2)
}
zdt2 <- function(x) {
  f1 <- x[1]
  g  <- 1 + 9 * sum(x[-1]) / (length(x) - 1)
  f2 <- g * (1 - (f1 / g)^2)
  c(f1, f2)
}
zdt3 <- function(x) {
  f1 <- x[1]
  g  <- 1 + 9 * sum(x[-1]) / (length(x) - 1)
  f2 <- g * (1 - sqrt(f1 / g) - (f1 / g) * sin(10 * pi * f1))
  c(f1, f2)
}

dtlz1 <- function(x, m = 2) {
  n <- length(x); k <- n - m + 1
  xm <- x[(n - k + 1):n]
  g  <- 100 * (k + sum((xm - 0.5)^2 - cos(20*pi*(xm - 0.5))))
  f  <- numeric(m)
  for (i in 1:m) {
    prod_term <- 0.5 * (1 + g)
    if (m - i >= 1) prod_term <- prod_term * prod(x[1:(m - i)])
    if (i > 1)      prod_term <- prod_term * (1 - x[m - i + 1])
    f[i] <- prod_term
  }
  f
}

dtlz2 <- function(x, m = 2) {
  n <- length(x); k <- n - m + 1
  xm <- x[(n - k + 1):n]
  g  <- sum((xm - 0.5)^2)
  f  <- numeric(m)
  for (i in 1:m) {
    prod_term <- (1 + g)
    if (m - i >= 1) prod_term <- prod_term * prod(cos(x[1:(m - i)] * pi/2))
    if (i > 1)      prod_term <- prod_term * sin(x[m - i + 1] * pi/2)
    f[i] <- prod_term
  }
  f
}

# ----------------------------
# Reference PFs (IGD)
# ----------------------------
pf_zdt1 <- function(n = 2000) { f1 <- seq(0,1,length.out=n); cbind(f1, 1 - sqrt(f1)) }
pf_zdt2 <- function(n = 2000) { f1 <- seq(0,1,length.out=n); cbind(f1, 1 - f1^2) }
pf_zdt3 <- function(n = 4000) { f1 <- seq(0,1,length.out=n); cbind(f1, 1 - sqrt(f1) - f1*sin(10*pi*f1)) }

pf_dtlz_sample <- function(fun, n_var, n = 30000, seed = 2026) {
  set.seed(seed)
  X <- matrix(runif(n * n_var), nrow = n, ncol = n_var)
  F <- t(apply(X, 1, fun))
  F <- as.matrix(F)
  storage.mode(F) <- "numeric"
  F <- F[complete.cases(F), , drop = FALSE]
  # take nondominated subset as PF approximation
  F <- F[emoa::nondominated_points(F), , drop = FALSE]
  F
}

# ----------------------------
# Strict 2D numeric matrix maker (or NULL)
# ----------------------------
as_F2_or_null <- function(F) {
  if (is.null(F)) return(NULL)
  F <- tryCatch(as.matrix(F), error = function(e) NULL)
  if (is.null(F)) return(NULL)
  storage.mode(F) <- "numeric"

  # Fix accidental 2xN
  if (nrow(F) == 2L && ncol(F) != 2L) F <- t(F)

  # Keep first 2 cols if extra were passed
  if (ncol(F) > 2L) F <- F[, 1:2, drop = FALSE]

  if (ncol(F) != 2L) return(NULL)

  F <- F[complete.cases(F), , drop = FALSE]
  if (nrow(F) < 2L) return(NULL)
  if (any(!is.finite(F))) return(NULL)
  F
}

# ----------------------------
# Robust HV (always returns number or NA)
# ----------------------------
hv_safe <- function(F, ref_point) {
  F <- as_F2_or_null(F)
  if (is.null(F)) return(NA_real_)

  ref_point <- as.numeric(ref_point)
  if (length(ref_point) != 2L || any(!is.finite(ref_point))) return(NA_real_)

  tryCatch(
    emoa::dominated_hypervolume(F, ref = ref_point),
    error = function(e) NA_real_
  )
}

# ----------------------------
# Manual IGD (stable + version-independent)
# IGD(PF_ref, F): average distance from each PF_ref point to nearest point in F
# ----------------------------
igd_manual <- function(F, PF_ref) {
  F <- as_F2_or_null(F)
  PF_ref <- as_F2_or_null(PF_ref)
  if (is.null(F) || is.null(PF_ref)) return(NA_real_)

  dists <- vapply(seq_len(nrow(PF_ref)), function(i) {
    dx <- F[, 1] - PF_ref[i, 1]
    dy <- F[, 2] - PF_ref[i, 2]
    min(sqrt(dx*dx + dy*dy))
  }, numeric(1))

  mean(dists)
}

# ----------------------------
# MO-Jaya: ONLY accept x1..xn_var from Pareto_Front
# If not present, return NULL (paper-safe)
# ----------------------------
run_mojaya <- function(obj_fun, n_var, lower, upper, popSize, maxiter) {
  res <- tryCatch(
    jaya_multi(
      objectives = list(function(x) obj_fun(x)[1], function(x) obj_fun(x)[2]),
      lower = lower, upper = upper,
      popSize = popSize, maxiter = maxiter,
      n_var = n_var
    ),
    error = function(e) NULL
  )
  if (is.null(res) || is.null(res$Pareto_Front)) return(NULL)

  PF <- as.data.frame(res$Pareto_Front, stringsAsFactors = FALSE)
  x_names <- paste0("x", 1:n_var)

  # Paper-clean rule: we only trust explicit x1..xn_var columns
  if (!all(x_names %in% names(PF))) return(NULL)

  X <- PF[, x_names, drop = FALSE]
  X <- suppressWarnings(as.matrix(apply(X, 2, as.numeric)))
  storage.mode(X) <- "numeric"
  X <- X[complete.cases(X), , drop = FALSE]
  if (nrow(X) < 2) return(NULL)

  # recompute objectives
  F <- matrix(NA_real_, nrow(X), 2)
  for (i in seq_len(nrow(X))) {
    y <- obj_fun(X[i, ])
    if (is.numeric(y) && length(y) == 2 && all(is.finite(y))) F[i, ] <- y
  }
  F <- as_F2_or_null(F)
  F
}

# ----------------------------
# NSGA-II
# ----------------------------
run_nsga2 <- function(obj_fun, n_var, lower, upper, popSize, generations) {
  res <- tryCatch(
    mco::nsga2(
      fn = obj_fun,
      idim = n_var, odim = 2,
      lower.bounds = lower, upper.bounds = upper,
      popsize = popSize, generations = generations
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  as_F2_or_null(res$value)
}

# ----------------------------
# Benchmarks
# ----------------------------
benchmarks <- list(
  list(name="ZDT1", fun=zdt1, n_var=30, bounds=make_bounds(30), pf=pf_zdt1()),
  list(name="ZDT2", fun=zdt2, n_var=30, bounds=make_bounds(30), pf=pf_zdt2()),
  list(name="ZDT3", fun=zdt3, n_var=30, bounds=make_bounds(30), pf=pf_zdt3()),
  list(name="DTLZ1", fun=function(x) dtlz1(x, m=2), n_var=7,  bounds=make_bounds(7),  pf=NULL),
  list(name="DTLZ2", fun=function(x) dtlz2(x, m=2), n_var=12, bounds=make_bounds(12), pf=NULL)
)

# Build PF refs
for (i in seq_along(benchmarks)) {
  if (is.null(benchmarks[[i]]$pf)) {
    benchmarks[[i]]$pf <- pf_dtlz_sample(benchmarks[[i]]$fun, benchmarks[[i]]$n_var, n=30000, seed=2026)
  }
  benchmarks[[i]]$pf <- as_F2_or_null(benchmarks[[i]]$pf)
  if (is.null(benchmarks[[i]]$pf)) stop("Reference PF invalid for: ", benchmarks[[i]]$name)
}

# ----------------------------
# Experiment controls
# ----------------------------
set.seed(2026)
n_runs      <- 20
popSize     <- 100
generations <- 200
maxiter     <- generations

algos <- c("MO-Jaya", "NSGA-II")
results <- list()

# ----------------------------
# Main loop
# ----------------------------
for (b in benchmarks) {
  cat("Benchmark:", b$name, "\n")
  lower <- b$bounds$lower
  upper <- b$bounds$upper

  PF_ref <- b$pf
  ref_point <- apply(PF_ref, 2, max) * 1.1  # consistent per benchmark

  for (algo in algos) {
    cat("  Algo:", algo, "\n")

    hv_vals  <- rep(NA_real_, n_runs)
    igd_vals <- rep(NA_real_, n_runs)

    for (r in 1:n_runs) {
      set.seed(1000 + r)

      F <- if (algo == "MO-Jaya") {
        run_mojaya(b$fun, b$n_var, lower, upper, popSize, maxiter)
      } else {
        run_nsga2(b$fun, b$n_var, lower, upper, popSize, generations)
      }

      hv_vals[r]  <- hv_safe(F, ref_point)
      igd_vals[r] <- igd_manual(F, PF_ref)
    }

    results[[length(results) + 1]] <- data.frame(
      Problem      = b$name,
      Algorithm    = algo,
      HV_mean      = mean(hv_vals, na.rm = TRUE),
      HV_sd        = sd(hv_vals,   na.rm = TRUE),
      HV_success   = mean(is.finite(hv_vals)),
      IGD_mean     = mean(igd_vals, na.rm = TRUE),
      IGD_sd       = sd(igd_vals,   na.rm = TRUE),
      IGD_success  = mean(is.finite(igd_vals))
    )
  }
}

summary_df <- do.call(rbind, results)
print(summary_df)
write.csv(summary_df, "results_summary.csv", row.names = FALSE)

# ----------------------------
# Pareto plot (ZDT1 example)
# ----------------------------
plot_problem <- "ZDT2"
b <- benchmarks[[which(vapply(benchmarks, `[[`, "", "name") == plot_problem)]]

set.seed(777)
F_jaya  <- run_mojaya(b$fun, b$n_var, b$bounds$lower, b$bounds$upper, popSize, maxiter)
F_nsga2 <- run_nsga2(b$fun, b$n_var, b$bounds$lower, b$bounds$upper, popSize, generations)

df_plot <- rbind(
  if (!is.null(F_jaya))  data.frame(f1 = F_jaya[,1],  f2 = F_jaya[,2],  Algorithm = "MO-Jaya"),
  if (!is.null(F_nsga2)) data.frame(f1 = F_nsga2[,1], f2 = F_nsga2[,2], Algorithm = "NSGA-II"),
  data.frame(f1 = b$pf[,1], f2 = b$pf[,2], Algorithm = "Reference PF")
)

p <- ggplot(df_plot, aes(x = f1, y = f2, shape = Algorithm)) +
  geom_point(alpha = 0.65, size = 1.6) +
  labs(title = paste0("Pareto Front Comparison on ", plot_problem),
       x = "Objective 1", y = "Objective 2") +
  theme_minimal(base_size = 12)

ggsave("pareto_plot.png", p, width = 7, height = 5, dpi = 300)

cat("\nSaved:\n - results_summary.csv\n - pareto_plot.png\n\n")

cat("NOTE:\n")
cat(" - If MO-Jaya shows HV_success=0, your Jaya::jaya_multi() Pareto_Front likely does not include x1..xn.\n")
cat("   For paper-quality MO-Jaya benchmarks, update jaya_multi() to return decision variables for Pareto solutions.\n")
cat(" - NSGA-II should show high success on ZDTs; if not, something is off in your local setup.\n")
