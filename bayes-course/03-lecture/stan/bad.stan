transformed data {
  vector[2] y;
  y[1] = -1;
  y[2] = 1;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 1000);
  sigma ~ exponential(0.0001);
  y ~ normal(mu, sigma);
}
