library(googlesheets4)
library(ggplot2)
library(glue)
library(rstanarm)
library(tidybayes)

data_mode <- Sys.getenv("TWO_TRUTHS_DATA_MODE", unset = "previous")
# Options: "current" for Y2026, "previous" for Y2024, or "both".
current_year_sheet <- "Y2026"
previous_year_sheet <- "Y2024"

thm <-
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    plot.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    panel.grid.major = element_blank()
  )
theme_set(thm)
gs4_deauth()

url1 <- "https://docs.google.com/spreadsheets/d/"
url2 <- "1oyCbed2BpQzHtvpA4DynTlPbwZpxOzOePNw9tas40qU/edit#gid=186692089"
url <- paste0(url1, url2)

data_mode <- match.arg(data_mode, choices = c("current", "previous", "both"))
selected_sheets <- switch(
  data_mode,
  current = current_year_sheet,
  previous = previous_year_sheet,
  both = c(current_year_sheet, previous_year_sheet)
)

read_two_truths <- function(sheet) {
  data <- read_sheet(url, sheet = sheet)
  colnames(data) <- c("time", "group", "score", "correct")
  data |>
    dplyr::mutate(
      year = sheet,
      time = as.character(time),
      group = as.character(group),
      score = as.numeric(score),
      correct = dplyr::case_when(
        correct == "Yes" ~ 1,
        correct == "No" ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::filter(!is.na(score), !is.na(correct))
}

data <- lapply(selected_sheets, read_two_truths) |>
  dplyr::bind_rows()

if (nrow(data) == 0) {
  stop(
    glue("No usable responses found in sheet(s): {toString(selected_sheets)}."),
    call. = FALSE
  )
}
data
n <- nrow(data)
set.seed(123)
data <- data |>
  dplyr::mutate(score_jitter = pmin(10, pmax(0, score + runif(n, -0.3, 0.3))))
data
p <- ggplot(aes(score_jitter, correct), data = data)
if (data_mode == "both") {
  p <- p + geom_point(aes(color = year))
} else {
  p <- p + geom_point()
}
p <- p +
  xlim(0, 10) +
  xlab("Certainty score") +
  ylab("Pr(Correct guess)")
p

# Fit using stan_glm
f1 <- stan_glm(
  correct ~ score,
  family = binomial(link = "logit"),
  data = data,
  refresh = 0,
  seed = 123
)
print(f1, digits = 2)

posterior_means <- coef(f1)
a_hat <- posterior_means["(Intercept)"]
b_hat <- posterior_means["score"]

make_epred_curves <- function(scores, model, n_draws = 50) {
  epred_draws <- data.frame(score = scores) |>
    add_epred_draws(model)
  draw_ids <- sample(
    unique(epred_draws$.draw),
    size = min(n_draws, dplyr::n_distinct(epred_draws$.draw))
  )

  list(
    posterior_lines = epred_draws |>
      dplyr::filter(.draw %in% draw_ids),
    mean_line = epred_draws |>
      dplyr::group_by(score) |>
      dplyr::summarize(.epred = mean(.epred), .groups = "drop")
  )
}

# Plot fitted curve
set.seed(123)
scores <- seq(0, 10, len = 30)
curves <- make_epred_curves(scores, f1)
p +
  geom_line(
    aes(score, .epred, group = .draw),
    linewidth = 0.2,
    alpha = 1 / 5,
    data = curves$posterior_lines
  ) +
  geom_line(
    aes(score, .epred),
    linewidth = 0.5,
    color = "red",
    data = curves$mean_line
  ) +
  geom_text(
    x = 3,
    y = 0.6,
    label = glue(
      "Pr(Y = 1) = invlogit({round(a_hat, 2)} + {round(b_hat, 2)}*x)"
    ),
    color = "brown",
    size = 5
  )

cat(
  "Probability of the correct guess ranges from",
  plogis(a_hat) |> round(2),
  "to",
  plogis(a_hat + b_hat * 10) |> round(2),
  "\n"
)

# plot in a larger range
from <- -10
to <- 20
scores <- seq(from, to, len = 30)
curves <- make_epred_curves(scores, f1)
p <- ggplot(aes(score_jitter, correct), data = data)
if (data_mode == "both") {
  p <- p + geom_point(aes(color = year))
} else {
  p <- p + geom_point()
}
p <- p +
  xlim(from, to) +
  xlab("Certainty score") +
  ylab("Pr(Correct guess)")
p +
  geom_line(
    aes(score, .epred, group = .draw),
    linewidth = 0.2,
    alpha = 1 / 5,
    data = curves$posterior_lines
  ) +
  geom_line(
    aes(score, .epred),
    linewidth = 0.5,
    color = "red",
    data = curves$mean_line
  )
