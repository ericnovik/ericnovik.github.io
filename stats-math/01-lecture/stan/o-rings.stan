functions {
  array[] int post_pred_rng(int N, vector alpha, vector beta, real x) {
    if (size(alpha) != size(beta)) {
      reject("alpha and beta must have the same size");
    }
    int n_draws = size(alpha);
    array[n_draws] int pred = binomial_rng(N, inv_logit(alpha + beta * x));
    return pred;
  }
}
data {
  int<lower=0> N_rows; // number of rows in data
  int<lower=0> N;      // number of possible "successes" in Binom(N, p)
  vector[N_rows] x;    // temperature for the O-Rings example
  array[N_rows] int<lower=0, upper=N> y; // number of "successes" in y ~ Binom(N, p)
}
parameters {
  real alpha;
  real beta; 
}
model {
  alpha ~ normal(0, 2.5);
  beta ~ normal(0, 1);
  y ~ binomial_logit(N, alpha + beta * x);
}


