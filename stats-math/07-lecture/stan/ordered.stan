data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  array[N] int<lower=1, upper=K> y;
  array[N] row_vector[D] x;
  //vector[K - 1] dir_priors;
}
parameters {
  vector[D] beta;
  ordered[K - 1] c;
  vector<lower=0>[K - 1] alpha;
}
model {
  beta ~ normal(0, 2);
  alpha ~ normal(0, 2);
  
  c ~ dirichlet(alpha);
  
  for (n in 1:N) {
    y[n] ~ ordered_logistic(x[n] * beta, c);
  }
}
generated quantities {
  array[N] int<lower=1, upper=K> yrep;
  
  for (n in 1:N) {
    yrep[n] = ordered_logistic_rng(x[n] * beta, c);
  }
}
