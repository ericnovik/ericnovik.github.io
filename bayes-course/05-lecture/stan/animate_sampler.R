library(cmdstanr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(magick)
library(readr)
library(scales)

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
out_path <- file.path(script_dir, "sampler-exploration.gif")

# Recreate d from the prior predictive section.
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
warmup_stride <- 2
sampling_stride <- 2

# Keep render settings low while iterating quickly.
draft_mode <- FALSE
fps <- if (draft_mode) 4 else 10
plot_width <- if (draft_mode) 960 else 1920
plot_height <- if (draft_mode) 540 else 1080

mod <- cmdstan_model(
  stan_file = stan_path,
  exe_file = file.path(tempdir(), "lin_reg_sampler")
)

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  save_warmup = TRUE,
  seed = 1234,
  refresh = 100
)

draws <- fit$draws(
  variables = c("alpha", "beta"),
  format = "df",
  inc_warmup = TRUE
) |>
  mutate(
    iter = .iteration,
    chain_id = .chain,
    phase = if_else(.iteration <= iter_warmup, "Warmup", "Sampling")
  ) |>
  group_by(chain_id) |>
  filter(
    (phase == "Warmup" & (.data$iter == 1 | .data$iter %% warmup_stride == 0)) |
      (phase == "Sampling" & (.data$iter == iter_warmup + 1 | .data$iter %% sampling_stride == 0))
  ) |>
  ungroup()

range_with_pad <- function(x, lower_q = 0.01, upper_q = 0.99, pad_frac = 0.08) {
  qs <- quantile(x, probs = c(lower_q, upper_q), na.rm = TRUE)
  span <- qs[2] - qs[1]
  if (span <= 0) span <- 1
  c(qs[1] - pad_frac * span, qs[2] + pad_frac * span)
}

build_cumulative_frames <- function(df) {
  frame_ids <- sort(unique(df$iter))
  frame_data <- lapply(frame_ids, function(fid) {
    dfi <- df |>
      filter(.data$iter <= fid) |>
      mutate(frame = fid, is_current = .data$iter == fid)
    dfi
  })
  bind_rows(frame_data)
}

make_hist_data <- function(cum_draws, x_limits, y_limits) {
  span_x <- diff(x_limits)
  span_y <- diff(y_limits)

  y_hist_base <- y_limits[1] - 0.24 * span_y
  y_hist_height <- 0.20 * span_y
  x_hist_base <- x_limits[2] + 0.02 * span_x
  x_hist_width <- 0.18 * span_x

  alpha_breaks <- seq(x_limits[1], x_limits[2], length.out = 35)
  beta_breaks <- seq(y_limits[1], y_limits[2], length.out = 35)

  alpha_hist <- cum_draws |>
    mutate(bin_id = cut(alpha, breaks = alpha_breaks, labels = FALSE, include.lowest = TRUE)) |>
    filter(!is.na(.data$bin_id)) |>
    group_by(.data$anim_frame, .data$chain_id, .data$bin_id) |>
    summarise(n = dplyr::n(), .groups = "drop")
  max_alpha_n <- max(alpha_hist$n)
  alpha_hist <- alpha_hist |>
    mutate(
      xmin = alpha_breaks[.data$bin_id],
      xmax = alpha_breaks[.data$bin_id + 1],
      ymin = y_hist_base,
      ymax = y_hist_base + (.data$n / max_alpha_n) * y_hist_height
    )

  beta_hist <- cum_draws |>
    mutate(bin_id = cut(beta, breaks = beta_breaks, labels = FALSE, include.lowest = TRUE)) |>
    filter(!is.na(.data$bin_id)) |>
    group_by(.data$anim_frame, .data$chain_id, .data$bin_id) |>
    summarise(n = dplyr::n(), .groups = "drop")
  max_beta_n <- max(beta_hist$n)
  beta_hist <- beta_hist |>
    mutate(
      ymin = beta_breaks[.data$bin_id],
      ymax = beta_breaks[.data$bin_id + 1],
      xmin = x_hist_base,
      xmax = x_hist_base + (.data$n / max_beta_n) * x_hist_width
    )

  list(
    alpha_hist = alpha_hist,
    beta_hist = beta_hist,
    x_plot_limits = c(x_limits[1], x_hist_base + 1.05 * x_hist_width),
    y_plot_limits = c(y_hist_base, y_limits[2])
  )
}

make_cummean_data <- function(seg_draws) {
  frame_ids <- sort(unique(seg_draws$iter))
  out <- lapply(frame_ids, function(fid) {
    dfi <- seg_draws |>
      filter(.data$iter <= fid)
    bind_rows(
      tibble(frame = fid, parameter = "alpha", cum_mean = mean(dfi$alpha)),
      tibble(frame = fid, parameter = "beta", cum_mean = mean(dfi$beta))
    )
  })
  bind_rows(out)
}

build_cummean_frames <- function(cum_means) {
  frame_ids <- sort(unique(cum_means$frame))
  out <- lapply(frame_ids, function(fid) {
    cum_means |>
      filter(.data$frame <= fid) |>
      mutate(anim_frame = fid, is_current = .data$frame == fid)
  })
  bind_rows(out)
}

combine_animations_vertical <- function(top_anim, bottom_anim) {
  n <- min(length(top_anim), length(bottom_anim))
  frames <- vector("list", n)
  for (i in seq_len(n)) {
    frames[[i]] <- image_append(c(top_anim[i], bottom_anim[i]), stack = TRUE)
  }
  image_join(frames)
}

sampling_only <- draws |> filter(phase == "Sampling")
warmup_only <- draws |> filter(phase == "Warmup")

# Wider global view for warmup.
xlim_warmup <- range_with_pad(draws$alpha, lower_q = 0.005, upper_q = 0.995, pad_frac = 0.05)
ylim_warmup <- range_with_pad(draws$beta, lower_q = 0.005, upper_q = 0.995, pad_frac = 0.05)

# Tight view around the posterior region after warmup.
xlim_sampling <- range_with_pad(sampling_only$alpha, lower_q = 0.01, upper_q = 0.99, pad_frac = 0.12)
ylim_sampling <- range_with_pad(sampling_only$beta, lower_q = 0.01, upper_q = 0.99, pad_frac = 0.12)

make_segment_animation <- function(seg_draws, x_limits, y_limits, phase_label) {
  is_sampling_phase <- all(seg_draws$phase == "Sampling")
  subtitle_text <- if (is_sampling_phase) {
    paste0("Iteration {current_frame}/", iter_sampling, " | Phase: ", phase_label)
  } else {
    paste0("Iteration {current_frame}/", iter_warmup, " | Phase: ", phase_label)
  }
  cum_draws <- build_cumulative_frames(seg_draws)
  if (is_sampling_phase) {
    cum_draws <- cum_draws |> mutate(anim_frame = .data$frame - iter_warmup)
  } else {
    cum_draws <- cum_draws |> mutate(anim_frame = .data$frame)
  }
  first_frame <- min(cum_draws$frame)
  h <- make_hist_data(cum_draws, x_limits, y_limits)
  cum_means <- make_cummean_data(seg_draws)
  mean_frames <- build_cummean_frames(cum_means)
  mean_plot_height <- if (draft_mode) 220 else 360
  chain_cols <- c("1" = "#0072B2", "2" = "#D55E00", "3" = "#009E73", "4" = "#CC79A7")
  mean_cols <- c("alpha" = "#1B9E77", "beta" = "#D95F02")

  # Put alpha and beta cumulative means on a shared visual scale with dual axes.
  alpha_means <- cum_means |> filter(.data$parameter == "alpha") |> pull(.data$cum_mean)
  beta_means <- cum_means |> filter(.data$parameter == "beta") |> pull(.data$cum_mean)
  alpha_range <- range_with_pad(alpha_means, lower_q = 0.02, upper_q = 0.98, pad_frac = 0.08)
  beta_range <- range_with_pad(beta_means, lower_q = 0.02, upper_q = 0.98, pad_frac = 0.08)

  alpha_span <- alpha_range[2] - alpha_range[1]
  beta_span <- beta_range[2] - beta_range[1]
  if (alpha_span <= 0) alpha_span <- 1
  if (beta_span <= 0) beta_span <- 1

  alpha_to_beta <- function(a) (a - alpha_range[1]) / alpha_span * beta_span + beta_range[1]
  alpha_long_run <- mean(seg_draws$alpha)
  beta_long_run <- mean(seg_draws$beta)
  show_ref_lines <- all(seg_draws$phase == "Sampling")

  mean_frames <- mean_frames |>
    mutate(
      y_plot = if_else(.data$parameter == "alpha", alpha_to_beta(.data$cum_mean), .data$cum_mean)
    )
  if (is_sampling_phase) {
    mean_frames <- mean_frames |> mutate(anim_frame = .data$anim_frame - iter_warmup)
  }

  p_main <- ggplot(
    cum_draws,
    aes(
      x = alpha,
      y = beta,
      color = factor(.data$chain_id),
      group = .data$chain_id
    )
  ) +
    geom_rect(
      data = h$alpha_hist,
      inherit.aes = FALSE,
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = factor(.data$chain_id)
      ),
      alpha = 0.20,
      color = NA
    ) +
    geom_rect(
      data = h$beta_hist,
      inherit.aes = FALSE,
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = factor(.data$chain_id)
      ),
      alpha = 0.20,
      color = NA
    ) +
    stat_density_2d(
      data = sampling_only,
      inherit.aes = FALSE,
      aes(x = alpha, y = beta),
      color = "gray80",
      linewidth = 0.3,
      bins = 10
    ) +
    geom_path(
      data = dplyr::filter(cum_draws, .data$frame > first_frame),
      linewidth = 0.28,
      alpha = 0.45
    ) +
    geom_point(size = 1.0, alpha = 0.45) +
    geom_point(
      data = function(x) dplyr::filter(x, .data$is_current),
      size = 2.5,
      alpha = 1
    ) +
    coord_cartesian(xlim = h$x_plot_limits, ylim = h$y_plot_limits, clip = "off") +
    scale_fill_manual(values = chain_cols, guide = "none") +
    scale_color_manual(
      values = chain_cols,
      name = "Chain"
    ) +
    labs(
      title = "NUTS sampler exploring posterior",
      subtitle = subtitle_text,
      x = expression(alpha),
      y = expression(beta)
    ) +
    theme_minimal(base_size = 24) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f0f1eb", color = NA),
      plot.background = element_rect(fill = "#f0f1eb", color = NA),
      plot.title = element_text(size = 34),
      plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16)
    ) +
    transition_manual(frames = .data$anim_frame) +
    ease_aes("linear")

  p_means <- ggplot(
    mean_frames,
    aes(
      x = .data$frame,
      y = .data$y_plot,
      color = .data$parameter,
      group = .data$parameter
    )
  ) +
    {if (show_ref_lines) geom_hline(
      yintercept = beta_long_run,
      color = mean_cols[["beta"]],
      linewidth = 0.35,
      linetype = "longdash",
      alpha = 0.8
    )} +
    {if (show_ref_lines) geom_hline(
      yintercept = alpha_to_beta(alpha_long_run),
      color = mean_cols[["alpha"]],
      linewidth = 0.35,
      linetype = "longdash",
      alpha = 0.8
    )} +
    geom_line(
      data = function(x) dplyr::filter(x, .data$anim_frame > min(.data$anim_frame)),
      linewidth = 0.9,
      alpha = 0.95
    ) +
    geom_point(
      data = function(x) dplyr::filter(x, .data$is_current),
      size = 1.8
    ) +
    scale_color_manual(values = mean_cols, name = "Cumulative mean") +
    scale_y_continuous(
      name = "beta cumulative mean",
      sec.axis = sec_axis(
        ~ ((. - beta_range[1]) / beta_span) * alpha_span + alpha_range[1],
        name = "alpha cumulative mean"
      )
    ) +
    labs(x = "Iteration", y = NULL) +
    theme_minimal(base_size = 17) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f0f1eb", color = NA),
      plot.background = element_rect(fill = "#f0f1eb", color = NA),
      plot.margin = margin(t = 0, r = 12, b = 6, l = 8),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    ) +
    coord_cartesian(ylim = beta_range) +
    transition_manual(frames = .data$anim_frame)

  main_anim <- animate(
    p_main,
    nframes = n_distinct(cum_draws$frame),
    fps = fps,
    width = plot_width,
    height = plot_height,
    renderer = magick_renderer()
  )

  means_anim <- animate(
    p_means,
    nframes = n_distinct(cum_draws$frame),
    fps = fps,
    width = plot_width,
    height = mean_plot_height,
    renderer = magick_renderer()
  )

  combine_animations_vertical(main_anim, means_anim)
}

anim_warmup <- make_segment_animation(
  seg_draws = warmup_only,
  x_limits = xlim_warmup,
  y_limits = ylim_warmup,
  phase_label = "Warmup (global view)"
)

anim_sampling <- make_segment_animation(
  seg_draws = sampling_only,
  x_limits = xlim_sampling,
  y_limits = ylim_sampling,
  phase_label = "Sampling (zoomed view)"
)

anim <- c(anim_warmup, anim_sampling)
image_write(anim, path = out_path, format = "gif")
message("Saved animation to: ", out_path)

# Also transcode to a small H.264 MP4 (the slides embed the MP4, since the
# raw GIF easily exceeds GitHub's 100 MB per-file limit).
mp4_path <- sub("\\.gif$", ".mp4", out_path)
if (nzchar(Sys.which("ffmpeg"))) {
  status <- system2(
    "ffmpeg",
    args = c(
      "-y", "-i", shQuote(out_path),
      "-movflags", "+faststart",
      "-pix_fmt", "yuv420p",
      "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2",
      "-c:v", "libx264", "-crf", "23", "-preset", "slow",
      shQuote(mp4_path)
    ),
    stdout = FALSE, stderr = FALSE
  )
  if (status == 0) {
    message("Saved MP4 to: ", mp4_path)
  } else {
    warning("ffmpeg returned status ", status, "; MP4 was not produced.")
  }
} else {
  warning("ffmpeg not found on PATH; skipping MP4 transcode. ",
          "Install ffmpeg to keep stan/sampler-exploration.mp4 in sync.")
}
