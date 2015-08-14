---
title: "Getting Started with Stan"
layout: post
---



During a recent Bayesian Inference with Stan class, [Daniel Lee](https://www.linkedin.com/in/syclik), one of the Stan's core developers walked us through a series of exercises to help us get started with Stan. This post is largely based on his introduction to Stan.


## Installing Stan
If you do not have Stan installed and you are an R User, follow the [RStan Getting Started Instruction](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). You need to make sure you have a working C++ compiler installed; the directions are provided via the above link. For Mac OS X, the recommendation is to use **clang**. [Interfaces](http://mc-stan.org/interfaces/) also exist for Python, Matlab, Julia, and Stata and for those who prefer to work with the Stan compiler directly, there is Command Stan. In this post, I will only be discussing the R interface.

## Loading Stan
Stan program is a text file that you feed to the Stan compiler which in turn translates it into C++ code and compiles it on the native platform. One implication of this is that the compiled Stan object is platform dependent (as opposed to say a Java program that executes on the JVM). 

Once, RStan is installed the package is loaded the usual way:

```r
library(rstan)
```

```
## Loading required package: Rcpp
## rstan (Version 2.7.0-2, packaged: , GitRev: 05c3d0058b6a)
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```r
# avoids unnesessary model recompilation
rstan_options(auto_write = TRUE)

# enable multicore support for running chains in parallel
options(mc.cores = parallel::detectCores())
```

## Bernoulli Model
The simplest model we can try is perhaps based on a series of independant binary events, each with an unknown probability \\(\\theta\\). The joint model (likelihood) can be written as:

<div>
$$ p(\theta,y) = \prod_{n = 1}^{N}\theta^{y_{n}} * (1 - \theta)^{1- y_{n}} = \theta^{\sum_{n=1}^{N}y_{n}} * (1 - \theta)^{\sum_{n=1}^{N}(1- y_{n})} $$
</div>

We typically work on a log scale, and so:

<div>
$$ log (p(\theta,y)) = \sum_{n=1}^{N}y_{n}*log(\theta) + \sum_{n=1}^{N}(1-y_{n})*log(1-\theta) $$
</div>

This model can be coded in Stan in several ways, two of which are demonstrated below. In addition to specifying the likelihood it is a good practice to put known constraints on the data and prior distributions on the parameters.


```r
data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta; # theta must be between 0 and 1
}
model {
  theta ~ normal(0.5, 0.2); # prior belief about theta: 95% of the mass is between 0.1 and 0.9

  increment_log_prob(0); 
  for (n in 1:N)
    increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));

  # the above three lines are equivalent to the following sampling statement:
  # y ~ bernoulli(theta);  
}
```
This *increment\_log\_prob()* business may seem a bit strange at first, but comparing it to the second equation, makes it clear that it is simply building up the log likelihood one observation at a time. For well known models Stan implements convenient shortcuts (sometimes called "syntactic sugar") as sampling statements such as *y \~ bernoulli(theta)*. The cool thing is that you can define your own model and use *increment\_log\_prob()* loop to build it directly. This is one of the features that makes Stan a probabalistic programming language in a sense that it provides the primitives for defining your own model and priors and then takes care of computing the posterior.

To call the model from R, I will generate some data from the Binomial distribution with a known parameter \\( \theta = 0.7 \\) and ask Stan to recover this parameter. Incidentally this is a good general practice for starting the modeling process: create some fake data with known parameters and try to recover them. 


```r
# data
data <- list(N = 1000, y = rbinom(1000, 1, 0.7))

# MLE of theta
sum(data$y) / 1000
```

```
## [1] 0.72
```

```r
# run Stan model for 200 iterations (will need more for real models) and 4 markov chains
bern <- stan("../models/bernoulli.stan", data = data, iter = 200, chains = 4)

# exract theta
theta <- extract(bern, pars = 'theta')$theta

quantile(theta, probs = c(.025, .25, .50, .75, .975))
```

```
## 2.5%  25%  50%  75%  98% 
## 0.69 0.71 0.72 0.73 0.74
```

```r
# what is the probability that theta is between 0.68 and 0.72
mean(theta > 0.68 & theta < 0.72)
```

```
## [1] 0.49
```

We can examine the full posterior density of \\( \theta \\) using ggplot:

```r
ggplot2::qplot(theta, geom = "density")
```

![plot of chunk unnamed-chunk-4]({{ site.url }}/images/getting-started-unnamed-chunk-4-1.png) 

RStan has a convenient *pairs()* function that can be used to examine the posterior distribution of the parameters and all pairwise scatterplots.


```r
pairs(bern)
```

![plot of chunk unnamed-chunk-5]({{ site.url }}/images/getting-started-unnamed-chunk-5-1.png) 

You can also take a look at the MCMC traceplots, which is helpful for checking convergence.

```r
traceplot(bern)
```

![plot of chunk unnamed-chunk-6]({{ site.url }}/images/getting-started-unnamed-chunk-6-1.png) 

Last but not least, Shiny Stan is a Shiny application that contains lots of interesting exploratory features for your Stan objects. Once installed, it can be launched as follows:


```r
shinystan::launch_shinystan(bern)
```

If would like to play with this particular model you can access it [on shyniapps.io](https://ericnovik.shinyapps.io/Bernoulli_Model).
