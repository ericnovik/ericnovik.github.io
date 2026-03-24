library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(latex2exp)

theta  <- c(0.25, 0.50, 0.75)
prior  <- c(1/2, 1/3, 1/6)
#prior  <- c(1/3, 1/3, 1/3)
payoff <- c(10, 15, 30)
N <- 5
k <- 0:N
C <- 5       # cost to play     
extraC <- 0.5 # cost of extra ball

prop_lik <- function(N, k, theta) {
  choose(N, k) * theta^k * (1 - theta)^(N - k)
}

EV <- function(C, payoff, p) {
  payoff * p - C
}

post <- matrix(nrow = length(theta), ncol = N + 1)
marginal_lik <- numeric(N + 1)
for (i in 1:ncol(post)) {
  unnorm_post  <- prop_lik(N, k[i], theta) * prior
  m_lik <- sum(unnorm_post)
  post[, i] <- unnorm_post / m_lik
  marginal_lik[i] <- m_lik
}

p0 <- ggplot(data.frame(x = 0:N, y = marginal_lik), aes(x, y))
p0 <- p0 + geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = 0:N) +
  labs(
    title = "Marginal likelihood (f(y = k))",
    x = "Number of red balls"
  ) + theme_minimal()

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

plot_thetas <- function(net, k, anot = TRUE, dollars = TRUE, dash = 2) {
  p <- ggplot(net, aes(x = k, y = value, color = theta, group = theta)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    scale_x_continuous(breaks = 0:max(k)) +
    labs(
      x = "Number of Red Balls (k) Observed",
      y = "",
      color = expression(theta)
    ) +
    geom_vline(xintercept = dash, linewidth = 0.2, linetype = "dashed") +
    theme(panel.grid.minor = element_blank()) +
    ggtitle("Net Expected Payoffs for Each Candidate Theta")
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

p1 <- plot_thetas(net_wide, k) #+ geom_vline(xintercept = 6, linewidth = 0.2, linetype = "dashed")
p1.1 <- plot_thetas(wide_to_long(post), k, dollars = FALSE, anot = FALSE) +
  ggtitle(TeX(r"(f(\theta = k | y))"))

# Buying an extra ball
theta_x_post_red <- theta * post
colnames(theta_x_post_red) <- k + 1 
p_next_red <- colSums(theta_x_post_red)
p_next_green <- 1 - p_next_red

# extra ball is red
norm_next_red <- sweep(theta_x_post_red, MARGIN = 2, STATS = p_next_red, FUN = "/")
long <- wide_to_long(norm_next_red)
p2 <- plot_thetas(long, k + 1, dollars = FALSE, anot = FALSE, dash = 3) +
  ggtitle(expression("Posterior probability " * f(theta ~ "|" ~ red))) +
  xlab("Number of Red Balls Observed")

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

# subtract cost to play and and cost of an extra ball
net_E_payoff <- p_next_red * max_payoff_red + p_next_green * max_payoff_green - (C + extraC)
df <- data.frame(net_E_payoff_no_extra_ball, net_E_payoff, k = k)
p3 <- ggplot(df, aes(x = k, y = net_E_payoff_no_extra_ball)) +
  geom_line(color = "blue", linewidth = 0.5) +
  geom_line(aes(y = net_E_payoff), color = "orange", linewidth = 0.5) +
  geom_text(data = df[df$k == 1, ], aes(label = "Don't buy extra ball"),
            color = "blue", hjust = 0, vjust = -0.8, size = 3.5) +
  geom_text(data = df[df$k == 4, ], aes(y = net_E_payoff, label = "Buy extra ball"),
            color = "orange", hjust = 0, vjust = 1.5, size = 3.5) +
  scale_y_continuous(
    breaks = seq(floor(min(df$net_E_payoff)), 
                 ceiling(max(df$net_E_payoff)), by = 1),
    labels = dollar_format(prefix = "$")) +
  scale_x_continuous(breaks = 0:max(k)) +
  theme(panel.grid.minor = element_blank()) +
  ylab("Net Expected Payoff") +
  xlab("Number of Red Balls out of 5")
  ggtitle("Expected Value of Buying an Extra Ball")
  
best_strategy_values <- apply(net, 2, max)
P_k <- marginal_lik
Total_Game_Value <- sum(best_strategy_values * P_k)
#print(Total_Game_Value)
