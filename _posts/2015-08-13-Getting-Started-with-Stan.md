---
title: "Getting Started with Stan"
layout: post
---



During the recent Bayesian Inference with Stan class, [Daniel Lee](https://www.linkedin.com/in/syclik), one of the Stan's core developers walked us through a series of exercises to help us get started with Stan. This post is largely based on his introduction to Stan.


## Installing Stan
If you do not have Stan installed and you are an R User, follow the [RStan Getting Started Instruction](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). You need to make sure you have a working C++ compiler installed; the directions are provided via the above link. For Mac OS X, the recommendation is to use **clang**. [Interfaces](http://mc-stan.org/interfaces/) also exist for Python, Matlab, Julia, and Stata and for those who prefer to work with the Stan compiler directly, there is Command Stan. In this post, I will only be discussing the R interface.

## Loading Stan
Stan program is of course just a text file that you feed to the Stan compiler which in turn translates it into C++ code and compiles it on the native platform. One implication of this is that the compiled Stan object is platform dependent (as opposed to say a Java program that executes on the JVM). 

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
The simplest model we can try is perhaps a series of independant binary events, each with an unknown probability \\(\\theta\\). The joint model can be written as:

$$ p(\theta,y) = \prod_{n = 1}^{N}\theta^{y_{n}} * (1 - \theta)^{1- y_{n}} = \theta^{\sum_{n=1}^{N}y_{n}} * (1 - \theta)^{\sum_{n=1}^{N}(1- y_{n})} $$

We typically work on a log scale, and so:

$$ log (p(\theta,y)) = \sum_{n=1}^{N}y_{n}*log(\theta) + \sum_{n=1}^{N}(1-y_{n})*log(1-\theta) $$

We can encode this model in Stan directly as follows:


```r
data {
  int N;
  int y[N];
}
parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  increment_log_prob(0);
  for (n in 1:N)
    increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));
}
```

Incremementing log probability is one way to specify the model in Stan. This is the essence of computation in Stan: it all boils down to getting a distribution of log posterior over \\(\\theta\\) given fixed observed data [Need a bit of the explanation on how that generates the log prob distribution for different values of theta]

There is no prior on \\(\\theta\\), but it is constrained between 0 and 1 so the resulting prior is \\(\\theta \\sim Uniform(0, 1)\\)

Consider a simple case of estimating 

```r
data <- list(N = 1000, y = rbinom(1000, 1, 0.7))
sum(data$y) / 1000
```

```
## [1] 0.7
```

```r
bern <- stan("../models/bernoulli.stan", data = data)
bern
```

```
## Inference for Stan model: bernoulli.
## 4 chains, each with iter=2000; warmup=1000; thin=1; 
## post-warmup draws per chain=1000, total post-warmup draws=4000.
## 
##         mean se_mean   sd    2.5%     25%    50%     75%     98% n_eff
## theta    0.7    0.00 0.01    0.67    0.69    0.7    0.71    0.73  1048
## lp__  -610.0    0.02 0.68 -611.85 -610.16 -609.8 -609.57 -609.52  1700
##       Rhat
## theta    1
## lp__     1
## 
## Samples were drawn using NUTS(diag_e) at Thu Aug 13 20:30:29 2015.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```

```r
pairs(bern)
```

![plot of chunk unnamed-chunk-3]({{ site.url }}/images/getting-started-unnamed-chunk-3-1.png) 


```r
data {
  int<lower = 0> N;
  int<lower = 0, upper = 1> y[N];
}
parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  theta ~ normal(0.5, 0.2);
  y ~ bernoulli(theta);  
}
```
