---
title: "Getting Started with Stan"
layout: post
---



During a recent Bayesian Inference with Stan class, [Daniel Lee](https://www.linkedin.com/in/syclik), one of the Stan's core developers walked us through a series of exercises to help us get started with Stan. This post is largely based on his introduction to Stan.


## Installing Stan
If you do not have Stan installed and you are an R User, follow the [RStan Getting Started Instruction](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). You need to make sure you have a working C++ compiler installed; the directions are provided via the above link. For Mac OS X, the recommendation is to use [clang](http://clang.llvm.org/). [Interfaces](http://mc-stan.org/interfaces/) also exist for Python, Matlab, Julia, and Stata and for those who prefer to work with the Stan compiler directly, there is Command Stan. In this post, I will only be discussing the R interface.

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
The simplest model we can try is perhaps based on a series of independent binary events, each with an unknown probability \\(\\theta\\) say when someone "hands you" a series of zeros and ones and asks to estimate the posterior probability of \\(\\theta > 0.50\\). The joint model (likelihood) can be written as:

<div>
$$ p(\theta,y) = \prod_{n = 1}^{N}\theta^{y_{n}} * (1 - \theta)^{1- y_{n}} = \theta^{\sum_{n=1}^{N}y_{n}} * (1 - \theta)^{\sum_{n=1}^{N}(1- y_{n})} $$
</div>

We typically work on a log scale, and so:

<div>
$$ \log (p(\theta,y)) = \sum_{n=1}^{N}y_{n}*\log(\theta) + \sum_{n=1}^{N}(1-y_{n})*\log(1-\theta) $$
</div>

This model can be coded in Stan in several ways. The following is my first, problematic attempt at doing so. (It will run and produce a reasonable answer, but it is neither well coded nor consistent with the math.) So, you have been warned -- do not do it this way!


```r
data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real theta;
}
model {
  theta ~ normal(0.5, 0.2); # prior on theta: 95% of the mass is between 0.1 and 0.9

  increment_log_prob(0); 
  for (n in 1:N)
    increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));

  # the above three lines are equivalent to the following sampling statement:
  # y ~ bernoulli(theta);  
}
```
This `increment_log_prob()` business may seem a bit strange at first, but comparing it to the second equation, makes it clear that it is simply building up the log likelihood one observation at a time. For well known models Stan implements convenient shortcuts (sometimes called "syntactic sugar") as sampling statements such as `y ~ bernoulli(theta)`. The cool thing is that you can define your own model and use `increment_log_prob()` loop to build it directly. This is one of the features that makes Stan a probabilistic programming language in a sense that it provides the primitives for specifying custom models and takes care of computing the posterior.

When I showed this model to [Bob](https://bob-carpenter.github.io/), he pretty much tore it apart, but I found his comments so instructive that I thought I would reproduce them here, so that the reader can contrast them with my half-assed representation.

## Model and Computational Criticism
The main criticism of the Stan model is that it does not correspond to the join model in the first equation in that nothing is multiplied by the normal prior as in the Stan code. Normal prior is a bit strange for \\( \theta \\) anyway.  If we change the Stan code to reflect a `uniform(0, 1)` prior on \\( \theta \\) then we get a consistent representation implicitly. Explicitly the likelihood would be multiplied by 1, which under the `log` transformation would result in adding a zero. This, incidentally, what `increment_log_prob(0)` was supposed to represent, but as Bob pointed out, this is a [no-op](https://en.wikipedia.org/wiki/NOP) and should be omitted.

A more traditional prior on a probability parameter would be a `beta` distribution. Stan does not take advantage of conjugacy and for simplicity and consistency, I will leave it as uniform (of course we could say `beta(1, 1)`.) Here is what the second iteration of the program looks like.


```r
data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta; # uniform(0, 1)
}
model {
  for (n in 1:N)
    increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));
}
```
Once we put in the constraints on \\( \theta \\) Stan will assign a `uniform(0, 1)` prior by defualt.

In the following, I will reproduce the rest of Bob's comments verbatim.


```r
for (n in 1:N) 
  increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));
```
is really bad for multiple reasons.  One, never use `log(1 - x)`, always use `log1m(x)` (this has to do with speed and computational stability.)  Second, you don't want to be multiplying by constant zeroes. So if you must use `increment_log_prob`, do it this way:


```r
for (n in 1:N)
    if (y[n] == 1)
      increment_log_prob(log(theta));
    else
      increment_log_prob(log1m(theta));
```

This is a nifty feature underscoring Stan's imperative programming nature! You can have conditional statements in the model. I like. OK, onto the rest of Bob's comments.

But even that's a bad idea (referring to the above code block.)  You want to assign `log(theta)` and `log1m(theta)` to constants so you don't have to recompute them.

So the program becomes:

```r
data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta; 
}
model {
  real log_theta;
  real log1m_theta;
  log_theta <- log(theta);
  log1m_theta <- log1m(theta);
  for (n in 1:N)
    if (y[n] == 1)
      increment_log_prob(log_theta);
   else
     increment_log_prob(log1m_theta);
}
```

Which can be written simply as:


```r
data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  y ~ bernoulli(theta);
}
```

## Running Stan Program from R
To call the Stan program from R, first create a new file called `bernoulli.stan` and place the model code in the file. If you are using RStudio, you will get some basic formatting and syntax highlighting. You will also be able to check the model for syntactic correctness.

<!---
<p align="center">
  <img src="images/rstudio.png" alt="RStudio" style="width:304px;height:228px;">
</p>
-->
![RStudio]({{ site.url }}/images/rstudio.png)

Next, generate some data from the Binomial distribution with a known parameter \\( \theta = 0.6 \\) and ask Stan to recover this parameter. Incidentally, this is a good general practice for starting the modeling process: create some fake data with known parameters and try to recover them. 


```r
# data
set.seed(1)
N <- 100
data <- list(N = N, y = rbinom(N, 1, 0.6))

# MLE of theta
sum(data$y) / N
```

```
## [1] 0.57
```

```r
# run Stan model for 200 iterations (will need more for real models) and 4 markov chains
bern <- stan("../models/bernoulli.stan", data = data, iter = 200, chains = 4)

# extract theta
theta <- extract(bern, pars = 'theta')$theta

quantile(theta, probs = c(.025, .25, .50, .75, .975))
```

```
## 2.5%  25%  50%  75%  98% 
## 0.49 0.55 0.58 0.61 0.65
```

```r
# what is the probability that theta is greater than 0.5
mean(theta > 0.5)
```

```
## [1] 0.94
```

```r
# what is the probability that theta is between 0.58 and 0.62
mean(theta > 0.58 & theta < 0.62)
```

```
## [1] 0.34
```

We can examine the full posterior density of \\( \theta \\) using ggplot:


```r
ggplot(as.data.frame(theta), aes(x = theta)) + geom_line(stat = "density")
```

<img src="{{ site.url }}/images/getting-started-unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />

Even better, RStan has a convenient `pairs()` function that can be used to examine the posterior distribution of the parameters and all the pairwise scatterplots.


```r
pairs(bern)
```

<img src="{{ site.url }}/images/getting-started-unnamed-chunk-10-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />

You can also take a look at the MCMC traceplots, which is helpful for checking convergence.

```r
traceplot(bern)
```

<img src="{{ site.url }}/images/getting-started-unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

Last but not least, Shiny Stan is a [Shiny](http://shiny.rstudio.com/) application that contains lots of interesting features for exploring your Stan objects. Once installed, it can be launched as follows:


```r
shinystan::launch_shinystan(bern)
```

If you would like to play with this particular (boring) model you can access it [on shyniapps.io](https://ericnovik.shinyapps.io/Bernoulli_Model).

For a more comprehensive introduction to Stan, see [here](http://www.stat.columbia.edu/~gelman/research/published/stan_jebs_2.pdf) and [here](http://www.stat.columbia.edu/~gelman/research/published/stan-paper-revision-feb2015.pdf).
