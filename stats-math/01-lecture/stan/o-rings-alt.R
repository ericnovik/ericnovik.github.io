library(cmdstanr)
library(posterior)
library(distributional)
library(tidybayes)
library(dplyr)
library(ggplot2)

file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-only.data"
d <- readr::read_table(file, col_names = FALSE)
colnames(d) <- c("n_rings", "n_distress", "temp", "psi", "flight_order")

d <- d |>
  mutate(not_in_distress = n_rings - n_distress,
         temp10 = temp/10)

m1 <- cmdstan_model(stan_file = "stats-math/01-lecture/stan/o-rings.stan")
#m1$compile(force_recompile = TRUE)
m1$expose_functions(global = TRUE)
data <- list(x = d$temp10, N_rows = nrow(d), N = 6, y = d$n_distress)
f1 <- m1$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.98
)
f1$summary()
draws <- as_draws_df(f1)
temp <- 20:81/10

library(purrr)
preds <- temp |>
  map(\(x) post_pred_rng(
    N = 6,
    alpha = draws$alpha,
    beta = draws$beta,
    x = x
  ))

newdata <-
  tibble(temp = temp * 10, ) |>
  bind_cols(sapply(preds, quantile, probs = c(0.05, 0.5, 0.95)) |> t())

library(tidybayes)
draws1 <- spread_draws(f1, alpha, beta) |>
  tidyr::expand_grid(x = d$temp10) |>
  mutate(pred = post_pred_rng(
    N = 6,
    alpha = alpha,
    beta = beta,
    x = x
  ))

p <- ggplot(newdata, aes(temp, `50%`))
p + geom_point(color = 'blue', alpha = 1/2) + xlab("Temperature at launch (F)") + ylab("") +
  geom_point(aes(temp, n_distress), data = d) +
  geom_linerange(aes(ymin = `5%`, ymax = `95%`), linewidth = 0.2) +
  ggtitle("Challenger Space Shuttle O-Ring Data",
          subtitle = "Number of O-rings experiencing thermal distress") +
  scale_y_continuous(breaks = 0:6)

