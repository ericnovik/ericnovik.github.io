library(googlesheets4)
library(ggplot2)
library(glue)
thm <-
  theme_minimal() + theme(
    panel.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    plot.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    panel.grid.major = element_blank()
  )
theme_set(thm)

# Sketch what they think the scatterplot and fitted curve for the class will look like, 
# and then we lead the class in discussion. Some possible prompts include: 
# What do you think the range of certainty scores will look like: will there be any 0’s or 10’s? 
# Will there be a positive relation between x and y: are guesses with higher certainty be 
# more accurate, on average? How strong will the relation be between x and y: 
# what will the curve look like?

url <- 'https://docs.google.com/spreadsheets/d/1oyCbed2BpQzHtvpA4DynTlPbwZpxOzOePNw9tas40qU/edit#gid=186692089'
data <- read_sheet(url)
colnames(data) <- c('time', 'group', 'score', 'correct')
data
n <- nrow(data)
data <- data |> dplyr::mutate(correct = ifelse(correct == "Yes", 1, 0),
                              score_jitter = score + runif(n, -0.3, 0.3))
data
p <- ggplot(aes(score_jitter, correct), data = data)
p <- p + geom_point() + xlim(0, 10) + 
  xlab("Certainty score") + 
  ylab("Pr(Correct guess)")
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
p + geom_line(aes(scores, logit), linewidth = 0.2, data = fitted) +
  geom_text(x = 3, 
            y = 0.6, 
            label = glue('Pr(Y = 1) = invlogit({round(a_hat, 2)} + {round(b_hat, 2)}*x)'),
            color = 'brown',
            size = 5)

cat("Probability of the correct guess ranges from", arm::invlogit(a_hat) 
    |> round(2), "to", arm::invlogit(a_hat + b_hat * 10) |> round(2))

# plot in a larger range
from <- -10; to <- 20
scores <- seq(from, to, len = 30)
fitted <- data.frame(logit = arm::invlogit(a_hat + b_hat * scores),
                     scores = scores)
p <- ggplot(aes(score_jitter, correct), data = data)
p <- p + geom_point() + xlim(from, to) + 
  xlab("Certainty score") + ylab("Pr(Correct guess)")
p + geom_line(aes(scores, logit), linewidth = 0.2, data = fitted)
