data {
  int<lower=1> N;
  array[N] int<lower=0> y;
}
parameters {
  real alpha;
  real<lower=0> phi_inv_sqrt;
}
transformed parameters {
  real<lower=0> phi = 1 / square(phi_inv_sqrt);
  real<lower=0> lambda = exp(alpha);
}
model {
  alpha ~ normal(0, 2);
  phi_inv_sqrt ~ normal(0, 1);

  y ~ neg_binomial_2_log(alpha, phi);
}
generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;
  real zero_prop_rep;
  real mean_rep;
  int max_rep;

  for (i in 1:N) {
    y_rep[i] = neg_binomial_2_log_rng(alpha, phi);
    log_lik[i] = neg_binomial_2_log_lpmf(y[i] | alpha, phi);
  }

  zero_prop_rep = 0;
  mean_rep = 0;
  max_rep = y_rep[1];
  for (i in 1:N) {
    zero_prop_rep += y_rep[i] == 0;
    mean_rep += y_rep[i];
    if (y_rep[i] > max_rep) max_rep = y_rep[i];
  }
  zero_prop_rep /= N;
  mean_rep /= N;
}
