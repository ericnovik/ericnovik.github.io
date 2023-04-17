data {
  int<lower=0> N;
  vector[N] w;
  vector[N] h;
  int<lower=0, upper=1> prior_PD;
}
transformed data {
  vector[N] w_m = w / mean(w);
  vector[N] h_m = h / mean(h);
}
parameters {
  real<lower=0> theta;
  real<lower=0> sigma;
  real<lower=0> h_pow;
}
transformed parameters {
 vector[N] mu = log(theta) + h_pow * log(h_m);
 //vector[N] exp(mu) = theta * pow(h, 3);
}
model {
  theta ~ normal(0, 1);
  sigma ~ exponential(1);
  h_pow ~ normal(3, 0.3);

  if (!prior_PD) {
    target += lognormal_lpdf(w_m | mu, sigma);
  }
}
generated quantities {
  array[N] real w_tilde = lognormal_rng(mu, sigma);
  array[N] real log_lik;
  
  for (n in 1:N) {
    log_lik[n] = lognormal_lpdf(w_m[n] | mu, sigma);
  }
}
