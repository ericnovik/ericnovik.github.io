library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

theta  <- c(0.25, 0.50, 0.75)
prior  <- c(1/3, 1/3, 1/3)
payoff <- c(3, 5, 15)
N <- 10
k <- 0:N
C <- 2      # cost to play     
extraC <- 1 # cost of extra ball

prop_lik <- function(N, k, theta) {
  theta^k * (1 - theta)^(N - k)
}

EV <- function(C, payoff, p) {
  payoff * p - C
}

post <- matrix(nrow = length(theta), ncol = N + 1)
for (i in 1:ncol(post)) {
  unnorm_post  <- prop_lik(N, k[i], theta) * prior
  post[, i] <- unnorm_post / sum(unnorm_post)
}

rownames(post) <- as.character(theta)
colnames(post) <- k

net <- EV(C, payoff, post)
net_E_payoff_no_extra_ball <- apply(net, 2, max)

wide_to_long <- function(w) {
  df <- as.data.frame(w)
  df <- df |> 
    mutate(theta = rownames(w))  
  long <- df |>
    pivot_longer(
      cols = -theta,           
      names_to = "k",          
      values_to = "value" 
    )
  long <- long |>
    mutate(k = as.numeric(k),
           theta = as.factor(theta))
  return(long)
}

net_wide <- wide_to_long(net)

plot_thetas <- function(net, k, anot = TRUE, dollars = TRUE) {
  p <- ggplot(net, aes(x = k, y = value, color = theta, group = theta)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    scale_x_continuous(breaks = 0:max(k)) +
    labs(
      x = "Number of Red Balls Observed in 10 Trials",
      y = "",
      color = expression(theta)
    ) +
    theme(panel.grid.minor = element_blank()) +
    ggtitle("Net Expected Payoffs for Each Candidate Theta",
            subtitle = "Under uniform prior on theta, C = $2, reward: {$3, $5, $15}")
  if (dollars) {
    p <- p + scale_y_continuous(
      breaks = seq(floor(min(net$value)), 
                   ceiling(max(net$value)), by = 1),
      labels = dollar_format(prefix = "$")
    ) 
  }
  if (anot) {
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, 
             fill = "red", alpha = 0.1) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, 
             fill = "green", alpha = 0.1)
  }
  return(p)
}

p1 <- plot_thetas(net_wide, k) + geom_vline(xintercept = 6, size = 0.2, linetype = "dashed")

# Buying an extra ball, so 11 trials
theta_x_post_red <- theta * post
colnames(theta_x_post_red) <- k + 1 
p_next_red <- colSums(theta_x_post_red)
p_next_green <- 1 - p_next_red

# extra ball is red
norm_next_red <- sweep(theta_x_post_red, MARGIN = 2, STATS = p_next_red, FUN = "/")
long <- wide_to_long(norm_next_red)
p2 <- plot_thetas(long, k + 1, dollars = FALSE, anot = FALSE) +
  ggtitle("Posterior probability f(theta | extra red ball)", subtitle = "") +
  xlab("Number of Red Balls Observed in 11 Trials") +
  geom_vline(xintercept = 7, size = 0.2, linetype = "dashed")

# computed expected payoff under extra red ball
E_payoff_red <- norm_next_red
for (i in 1:length(payoff)) {
  E_payoff_red[i, ] <- E_payoff_red[i, ] * payoff[i]
}

# extra ball is green
theta_x_post_geen <- (1 - theta) * post
colnames(theta_x_post_geen) <- k + 1 
norm_next_green <- sweep(theta_x_post_geen, MARGIN = 2, STATS = p_next_green, FUN = "/")

# computed expected payoff under extra green ball
E_payoff_green <- norm_next_green
for (i in 1:length(payoff)) {
  E_payoff_green[i, ] <- E_payoff_green[i, ] * payoff[i]
}

max_payoff_red <- apply(E_payoff_red, 2, max)
max_payoff_green <- apply(E_payoff_green, 2, max)

# subtract $2 to play and $1 for an extra ball
net_E_payoff <- p_next_red * max_payoff_red + p_next_green * max_payoff_green - 3
df <- data.frame(net_E_payoff_no_extra_ball, net_E_payoff, k = k)
p3 <- ggplot(df, aes(x = k, y = net_E_payoff_no_extra_ball)) +
  geom_line(color = "green", size = 0.5) +
  geom_line(aes(y = net_E_payoff), color = "red", size = 0.5) +
  scale_y_continuous(
    breaks = seq(floor(min(df$net_E_payoff)), 
                 ceiling(max(df$net_E_payoff)), by = 1),
    labels = dollar_format(prefix = "$")) +
  scale_x_continuous(breaks = 0:max(k)) +
  theme(panel.grid.minor = element_blank()) +
  ylab("Net Expected Payoff") +
  ggtitle("Expected Value of Buying and Extra Ball (Red), Green: No Extra Ball",
          subtitle = "Under uniform prior on theta, C = $2, reward: {$3, $5, $15}, extra ball = $1")
  
