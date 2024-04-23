data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  int<lower=0, upper=1> prior_PD;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu = alpha + beta * x;
}
model {
  alpha ~ normal(60, 10);
  beta ~ normal(0.55, 0.1);
  sigma ~ exponential(0.2);
  if (!prior_PD) {
    y ~ normal(mu, sigma);
  }
}
generated quantities {
  array[N] real yrep = normal_rng(mu, sigma);
}
