data {
  real y;
  real<lower=0> sigma;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 2);
  y ~ normal(mu, sigma);
}
generated quantities {
  real y_tilde = normal_rng(mu, sigma);
}
