library(cmdstanr)
library(loo)
library(posterior)
library(bayesplot)
library(ggplot2)

set.seed(123)

# Simulate data from a zero-inflated Poisson model.
n <- 400
lambda_true <- 1.5
zi_true <- 0.40

is_structural_zero <- rbinom(n, size = 1, prob = zi_true)
y <- ifelse(is_structural_zero == 1, 0, rpois(n, lambda_true))
stan_data <- list(N = n, y = y)

cat("Observed zero proportion:", round(mean(y == 0), 2), "\n")
cat("Observed mean:", round(mean(y), 2), "\n")
cat("Poisson zero probability at observed mean:", round(exp(-mean(y)), 2), "\n")

fit_args <- list(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 250
)

cat("\nFitting negative binomial model...\n")
nb <- do.call(cmdstan_model("neg_binomial.stan")$sample, fit_args)

cat("\nFitting zero-inflated Poisson model...\n")
zip <- do.call(cmdstan_model("zip.stan")$sample, fit_args)

cat("\nNegative binomial posterior summary:\n")
print(nb$summary(c("lambda", "phi"))[, c(
  "variable",
  "mean",
  "median",
  "q5",
  "q95",
  "rhat",
  "ess_bulk"
)])

cat("\nZero-inflated Poisson posterior summary:\n")
print(zip$summary(c("lambda", "zi"))[, c(
  "variable",
  "mean",
  "median",
  "q5",
  "q95",
  "rhat",
  "ess_bulk"
)])

cat("\nLOO comparison:\n")
print(loo_compare(nb$loo(), zip$loo()))

extract_yrep <- function(fit) {
  draws <- as_draws_matrix(fit$draws("y_rep"))
  colnames(draws) <- paste0("y_rep[", seq_len(n), "]")
  draws
}

yrep_nb <- extract_yrep(nb)
yrep_zip <- extract_yrep(zip)

color_scheme_set("brightblue")

p_nb_root <- bayesplot::ppc_rootogram(y, yrep_nb[1:100, ]) +
  ggtitle("Negative binomial posterior predictive rootogram")

p_zip_root <- bayesplot::ppc_rootogram(y, yrep_zip[1:100, ]) +
  ggtitle("Zero-inflated Poisson posterior predictive rootogram")

print(p_nb_root)
print(p_zip_root)
