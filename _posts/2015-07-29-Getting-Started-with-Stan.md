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


```r
data <- list(N = 1000, y = rbinom(1000, 1, 0.7))
sum(data$y) / 1000
```

```
## [1] 0.721
```

```r
bern <- stan("models/bernoulli.stan", data = data)
```

```
## Warning in file(fname, "rt"): cannot open file 'models/bernoulli.stan': No
## such file or directory
```

```
## Error in get_model_strcode(file, model_code): cannot open model file "models/bernoulli.stan"
```

```r
bern
```

```
## Error in eval(expr, envir, enclos): object 'bern' not found
```


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




```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

You can also embed plots, for example:

![plot of chunk unnamed-chunk-5]({{ site.url }}/images/getting-started-unnamed-chunk-5-1.png) 

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
