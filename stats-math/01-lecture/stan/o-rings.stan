data {
  int<lower=0> N_rows; // number of rows
  int<lower=0> N;      // number of trials in Binom(N, p)
  vector[N_rows] x;    // temperature for the O-Rings example
  array[N_rows] int<lower=0, upper=N> y; // number of "successes" in Binom(N, p)
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 1);
  y ~ binomial_logit(N, alpha + beta * x);
}

