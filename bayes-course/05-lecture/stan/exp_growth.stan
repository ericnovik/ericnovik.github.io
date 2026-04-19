data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real<lower=0> a;
  real b;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu = a * exp(b * x);
}
model {
  a     ~ normal(0, 1);
  b     ~ normal(0, 1);
  sigma ~ exponential(0.2);
  y ~ normal(mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma);
}
