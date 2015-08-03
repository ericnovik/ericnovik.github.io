---
title: "Getting Started with Stan"
layout: post
---



During the recent Bayesian Inference with Stan class, [Daniel Lee](https://www.linkedin.com/in/syclik), one of the Stan's core developers walked us through a series of exercises to help us get started with Stan. This post is my reproduction of his introduction to Stan.

## Installing Stan
If you do not have Stan installed and you are an R User, follow the [RStan Getting Started Instruction](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). You need to make sure you have a working C++ compiler installed; the directions are provided via the above link. For Mac OS X, the recommendation is to use **clang**. [Interfaces](http://mc-stan.org/interfaces/) also exist for Python, Matlab, Julia, and Stata and for those who prefer to work with the Stan compiler directly, there is Command Stan. In this post, I will only be discussing the R interface.

## Running Stan
Stan program is of course just a text file that you feed to the Stan compiler which translates it into C++ code and compiles it on the native platform. One implication of this is that the compiled Stan object is platform dependent (as opposed to say a Java program that executes on the JVM). 

Once, RStan is installed the package is loaded the usual way:

```r
library(rstan)
```

```
## Loading required package: Rcpp
## Loading required package: inline
## 
## Attaching package: 'inline'
## 
## The following object is masked from 'package:Rcpp':
## 
##     registerPlugin
## 
## rstan (Version 2.7.0-1, packaged: 2015-07-17 18:12:01 UTC, GitRev: 05c3d0058b6a)
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```r
# enable multicore support so chains can be fit in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```


Consider a simple case of estimating 

```r
data <- list(N = 1000, y = rbinom(1000, 1, 0.7))
sum(data$y) / 1000
```

```
## [1] 0.701
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
##          mean se_mean   sd    2.5%     25%    50%     75%   97.5% n_eff
## theta    0.70    0.00 0.01    0.67    0.69    0.7    0.71    0.73  1276
## lp__  -612.59    0.02 0.75 -614.67 -612.71 -612.3 -612.13 -612.08  1847
##       Rhat
## theta    1
## lp__     1
## 
## Samples were drawn using NUTS(diag_e) at Thu Jul 30 15:21:31 2015.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```

```r
pairs(bern)
```

![plot of chunk unnamed-chunk-2]({{ site.url }}/images/getting-started-unnamed-chunk-2-1.png) 


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
