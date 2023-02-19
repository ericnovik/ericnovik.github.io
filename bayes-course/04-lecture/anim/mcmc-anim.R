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

make_anim <- function(proposal, accepted, yscale_l, yscale_h, ...) {
  accepted <- as.factor(accepted)
  iter <- 1:length(proposal)
  anim <- ggplot(data.frame(iter, proposal), aes(iter, proposal)) +
    geom_line(aes(color = accepted), size = 0.1) + 
    geom_point(aes(seq_along(iter), color = accepted)) +
    geom_point(aes(y = proposal, color = accepted)) +
    ylim(yscale_l, yscale_h) +
    xlab("Iteration") + ylab(expression(mu)) +
    transition_reveal(iter)
  return(anim)
}

library(latex2exp)
library(glue)
library(gganimate)

y <- 5
N <- 500
start <- 3
ps <- 0.1
d1 <- mhr(y, N, f = metropolis_iteration, 
         start, proposal_scale = ps, sd = 0.5,  
         prior_mean = 0, prior_sd = 2)

ps <- 10
d2 <- mhr(y, N, f = metropolis_iteration, 
          start, proposal_scale = ps, sd = 0.5,  
          prior_mean = 0, prior_sd = 2)

ps <- 2
d3 <- mhr(y, N, f = metropolis_iteration, 
          start, proposal_scale = ps, sd = 0.5,  
          prior_mean = 0, prior_sd = 2)

save_anim <- function(d, ps, ysl, ysh) {
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

save_anim(d1, ps = 0.1, ysl = 2.5, ysh = 6)
save_anim(d2, ps = 10, ysl = -10, ysh = 20)
save_anim(d3, ps = 2, ysl = -10, ysh = 20)


