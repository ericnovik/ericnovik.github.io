library(googlesheets4)
library(ggplot2)
thm <-
  theme_minimal() + theme(
    panel.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    plot.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    panel.grid.major = element_blank()
  )
theme_set(thm)

# Read google sheets data into R
data <- read_sheet('https://docs.google.com/spreadsheets/d/1oyCbed2BpQzHtvpA4DynTlPbwZpxOzOePNw9tas40qU/edit#gid=186692089')
colnames(data) <- c('time', 'group', 'score', 'correct')
data
data <- data |> dplyr::mutate(correct = ifelse(correct == "Yes", 1, 0),
                              score_jitter = score + runif(n, -0.3, 0.3))
data
p <- ggplot(aes(score_jitter, correct), data = data)
p <- p + geom_point() + xlim(0, 10) + 
  xlab("Certainty score") + 
  ylab("Correct guess?")
p
# Fit using glm
f1 <- glm(correct ~ score,
          family = binomial(link = "logit"),
          data = data)
arm::display(f1)

# Plot fitted curve
a_hat <- coef(f1)["(Intercept)"]
b_hat <- coef(f1)["score"]
scores <- seq(0, 10, len = 30)
fitted <- data.frame(logit = arm::invlogit(a_hat + b_hat * scores),
                     scores = scores)
p + geom_line(aes(scores, logit), linewidth = 0.2, data = fitted)

# plot in a larger range
from <- -10; to <- 20
scores <- seq(from, to, len = 30)
fitted <- data.frame(logit = arm::invlogit(a_hat + b_hat * scores),
                     scores = scores)
p <- ggplot(aes(score_jitter, correct), data = data)
p <- p + geom_point() + xlim(from, to) + 
  xlab("Certainty score") + ylab("Correct guess?")
p + geom_line(aes(scores, logit), linewidth = 0.2, data = fitted)
