library(ggplot2)
library(gganimate)
library(ggplot2)
library(gganimate)
library(dplyr)
thm <-
  theme_minimal() + theme(
    panel.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    plot.background = element_rect(fill = "#f0f1eb", color = "#f0f1eb"),
    panel.grid.major = element_blank()
  )
theme_set(thm)

metropolis_iteration <- function(y, current_mean, proposal_scale, sd,
                                 prior_mean, prior_sd) {
  # assume prior ~ N(pm, psd)
  # proposal sampling distribution q = N(mu, proposal_scale)
  proposal_mean <- rnorm(1, current_mean,  proposal_scale)        
  f_proposal    <- dnorm(proposal_mean, mean = prior_mean, sd = prior_sd) * 
    dnorm(y, mean = proposal_mean, sd = sd)
  f_current     <- dnorm(current_mean, mean = prior_mean, sd = prior_sd) *        
    dnorm(y, mean = current_mean, sd = sd)
  
  ratio <- f_proposal / f_current 
  alpha <- min(ratio, 1)     
  
  accepted <- TRUE
  if (alpha > runif(1)) {    
    next_value <- proposal_mean
  } else {
    next_value <- current_mean
    accepted <- FALSE
  }
  return(c(next_value, proposal_mean, alpha, accepted))
}

mhr <- function(y, f, N, start, ...) {
  # y: observations, could be a vector
  # f: function that implements one MHR iteration
  # N: number of iterations
  # start: initial value of the chain
  # ...: additional arguments to f
  
  draws <- matrix(nrow = N, ncol = 4)
  draws[1, ] <- f(y, current_mean = start, ...)
  
  for (i in 2:N) {
    draws[i, ] <- f(y, current_mean = draws[i - 1, 1], ...)
  }
  
  colnames(draws) <- c("next", "proposal", "alpha", "accepted")
  return(draws)
}

make_plot <- function(df, title, ...) {
  p <- ggplot(df, aes(x = iteration)) +
    geom_line(aes(y = `next`), size = 0.1, color = "green") +
    geom_point(aes(y = proposal, color = factor(accepted)), size = 0.2) +
    scale_color_manual(
      values = c("0" = "red", "1" = "green"),
      labels = NULL,
      name = NULL
    ) +
    ylab("μ") +
    xlab("Iteration") +
    ggtitle(title) + theme(thm, legend.position = "none")
  return(p)
}
animate_mcmc <- function(p) {
  anim <- p + transition_manual(iteration, cumulative = TRUE)
  return(anim)
}
make_acf <- function(d, ps, ysl, ysh) {
  # str <- glue("Proposal distribution q: $Normal(\\mu' | \\mu, \\sigma = {ps})$")
  # anim <- make_anim(d[, 'proposal'],
  #                   d[, 'accepted'], ysl, ysh) + ggtitle(TeX(str))
  # animate(anim, fps = 5, detail = 3, height = 600, width = 800, res = 150)
  # anim_save(glue("anim-{ps}.gif"))
  # str <- glue("$Normal(\\mu' | \\mu, \\sigma = {ps})$")
  png(glue("anim-{ps}.png"), bg = "#f0f1eb", width = 800,
      height = 600, res = 200)
  par(mar = c(3,3,2,1), mgp = c(2,.7,0), tck = -.01, bg = "#f0f1eb")
  acf(d[, 1], ylab = "", main = "")
  dev.off()
}
y <- 5
N <- 500
start <- 3
ps <- c(0.1, 10, 2)
d1 <- mhr(y, N, f = metropolis_iteration, 
          start, proposal_scale = ps[1], sd = 0.5,  
          prior_mean = 0, prior_sd = 2)
d2 <- mhr(y, N, f = metropolis_iteration, 
          start, proposal_scale = ps[2], sd = 0.5,  
          prior_mean = 0, prior_sd = 2)
d3 <- mhr(y, N, f = metropolis_iteration, 
          start, proposal_scale = ps[3], sd = 0.5,  
          prior_mean = 0, prior_sd = 2)

d1 <- d1 |> as.data.frame() |> mutate(iteration = row_number())
d2 <- d2 |> as.data.frame() |> mutate(iteration = row_number())
d3 <- d3 |> as.data.frame() |> mutate(iteration = row_number())

ttl <- glue("Proposal distribution q: Normal(μ' | μ, σ = {ps[1]})")
p <- make_plot(d1, title = TeX(ttl))
chain_anim <- animate_mcmc(p)
anim_save("bayes-course/04-lecture/anim/mcmc_chain_1.gif", 
          animation = chain_anim, bg = "#f0f1eb",
          fps = 10, detail = 3, height = 600, width = 800, res = 150)

ttl <- glue("Proposal distribution q: Normal(μ' | μ, σ = {ps[2]})")
p <- make_plot(d2, title = TeX(ttl))
chain_anim <- animate_mcmc(p)
anim_save("bayes-course/04-lecture/anim/mcmc_chain_2.gif", 
          animation = chain_anim, bg = "#f0f1eb",
          fps = 10, detail = 3, height = 600, width = 800, res = 150)

ttl <- glue("Proposal distribution q: Normal(μ' | μ, σ = {ps[3]})")
p <- make_plot(d3, title = TeX(ttl))
chain_anim <- animate_mcmc(p)
anim_save("bayes-course/04-lecture/anim/mcmc_chain_3.gif", 
          animation = chain_anim, bg = "#f0f1eb",
          fps = 10, detail = 3, height = 600, width = 800, res = 150)



