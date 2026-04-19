library(cmdstanr)
library(dplyr)
library(plotly)
library(readr)
library(htmlwidgets)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(script_path) == 0) {
    return(normalizePath(getwd()))
  }
  dirname(normalizePath(script_path))
}

script_dir <- get_script_dir()
lecture_dir <- normalizePath(file.path(script_dir, ".."))

data_path <- file.path(lecture_dir, "data", "howell.csv")
stan_path <- file.path(script_dir, "lin_reg.stan")
out_html <- file.path(script_dir, "posterior-3d.html")

# Same data prep as lecture model setup: adults and centered height.
d <- read_csv(data_path, show_col_types = FALSE) |>
  filter(age >= 18) |>
  mutate(height_c = height - mean(height))

stan_data <- list(
  N = nrow(d),
  x = d$height_c,
  y = d$weight,
  prior_PD = 0
)

iter_warmup <- 200
iter_sampling <- 800

mod <- cmdstan_model(
  stan_file = stan_path,
  exe_file = file.path(tempdir(), "lin_reg_3d")
)

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  seed = 1234,
  refresh = 100
)

draws <- fit$draws(
  variables = c("alpha", "beta", "sigma"),
  format = "df",
  inc_warmup = FALSE
) |>
  as.data.frame() |>
  select(alpha, beta, sigma)

# Keep rendering responsive while preserving posterior shape.
max_points <- 8000
if (nrow(draws) > max_points) {
  set.seed(1234)
  draws <- draws |> slice_sample(n = max_points)
}

# Use Mahalanobis distance as a smooth proxy for relative density.
center <- colMeans(draws)
cov_mat <- cov(draws)
md2 <- mahalanobis(draws, center = center, cov = cov_mat)
rel_density <- scales::rescale(-md2, to = c(0, 1))

draws_plot <- draws |>
  mutate(rel_density = rel_density)

fig <- plot_ly() |>
  add_trace(
    data = draws_plot,
    x = ~alpha,
    y = ~beta,
    z = ~sigma,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 2,
      opacity = 0.55,
      color = ~rel_density,
      colorscale = "Viridis",
      cmin = 0,
      cmax = 1,
    showscale = FALSE
    ),
    showlegend = FALSE,
    hovertemplate = paste(
      "alpha: %{x:.3f}<br>",
      "beta: %{y:.4f}<br>",
      "sigma: %{z:.3f}<br>",
      "rel. density: %{marker.color:.3f}<extra></extra>"
    )
  ) |>
  add_trace(
    data = draws_plot,
    x = ~alpha,
    type = "histogram",
    nbinsx = 28,
    marker = list(color = "#4c78a8"),
    opacity = 0.9,
    xaxis = "x2",
    yaxis = "y2",
    showlegend = FALSE,
    hovertemplate = "alpha: %{x:.3f}<br>count: %{y}<extra></extra>"
  ) |>
  add_trace(
    data = draws_plot,
    x = ~beta,
    type = "histogram",
    nbinsx = 28,
    marker = list(color = "#f58518"),
    opacity = 0.9,
    xaxis = "x3",
    yaxis = "y3",
    showlegend = FALSE,
    hovertemplate = "beta: %{x:.4f}<br>count: %{y}<extra></extra>"
  ) |>
  add_trace(
    data = draws_plot,
    x = ~sigma,
    type = "histogram",
    nbinsx = 28,
    marker = list(color = "#54a24b"),
    opacity = 0.9,
    xaxis = "x4",
    yaxis = "y4",
    showlegend = FALSE,
    hovertemplate = "sigma: %{x:.3f}<br>count: %{y}<extra></extra>"
  ) |>
  layout(
    title = "Posterior draws in (alpha, beta, sigma)",
    showlegend = FALSE,
    paper_bgcolor = "#f0f1eb",
    plot_bgcolor = "#f0f1eb",
    margin = list(l = 10, r = 10, t = 50, b = 10),
    scene = list(
      domain = list(x = c(0.00, 0.80), y = c(0.00, 1.00)),
      bgcolor = "#f0f1eb",
      xaxis = list(title = "alpha"),
      yaxis = list(title = "beta"),
      zaxis = list(title = "sigma"),
      camera = list(eye = list(x = 1.65, y = 1.45, z = 0.95))
    ),
    xaxis2 = list(domain = c(0.84, 0.99), anchor = "y2", title = ""),
    yaxis2 = list(domain = c(0.72, 0.92), anchor = "x2", title = "", showticklabels = FALSE, ticks = ""),
    xaxis3 = list(domain = c(0.84, 0.99), anchor = "y3", title = ""),
    yaxis3 = list(domain = c(0.41, 0.61), anchor = "x3", title = "", showticklabels = FALSE, ticks = ""),
    xaxis4 = list(domain = c(0.84, 0.99), anchor = "y4", title = ""),
    yaxis4 = list(domain = c(0.10, 0.30), anchor = "x4", title = "", showticklabels = FALSE, ticks = ""),
    annotations = list(
      list(text = "alpha", x = 0.915, y = 0.94, xref = "paper", yref = "paper", showarrow = FALSE),
      list(text = "beta",  x = 0.915, y = 0.63, xref = "paper", yref = "paper", showarrow = FALSE),
      list(text = "sigma", x = 0.915, y = 0.32, xref = "paper", yref = "paper", showarrow = FALSE)
    )
  )

saveWidget(fig, file = out_html, selfcontained = TRUE)
message("Saved interactive plot to: ", out_html)
