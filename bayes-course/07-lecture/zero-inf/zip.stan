data {
  int<lower=1> N;
  array[N] int<lower=0> y;
}
parameters {
  real alpha;
  real<lower=0, upper=1> zi;
}
model {
  alpha ~ normal(0, 2);
  zi ~ beta(1, 3);
  
  for (i in 1 : N) {
    if (y[i] == 0) {
      target += log_sum_exp(bernoulli_lpmf(1 | zi),
                            bernoulli_lpmf(0 | zi)
                            + poisson_log_lpmf(0 | alpha));
    } else {
      target += bernoulli_lpmf(0 | zi) + poisson_log_lpmf(y[i] | alpha);
    }
  }
}
generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;
  real zero_prop_rep;
  real mean_rep;
  int max_rep;
  real<lower=0> lambda = exp(alpha);
  
  for (i in 1 : N) {
    if (bernoulli_rng(zi) == 1) {
      y_rep[i] = 0;
    } else {
      y_rep[i] = poisson_log_rng(alpha);
    }
    
    if (y[i] == 0) {
      log_lik[i] = log_sum_exp(bernoulli_lpmf(1 | zi),
                               bernoulli_lpmf(0 | zi)
                               + poisson_log_lpmf(0 | alpha));
    } else {
      log_lik[i] = bernoulli_lpmf(0 | zi) + poisson_log_lpmf(y[i] | alpha);
    }
  }
  
  zero_prop_rep = 0;
  mean_rep = 0;
  max_rep = y_rep[1];
  for (i in 1 : N) {
    zero_prop_rep += y_rep[i] == 0;
    mean_rep += y_rep[i];
    if (y_rep[i] > max_rep) 
      max_rep = y_rep[i];
  }
  zero_prop_rep /= N;
  mean_rep /= N;
}
