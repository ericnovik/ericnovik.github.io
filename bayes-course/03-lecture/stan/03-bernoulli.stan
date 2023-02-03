// assuming beta(2, 2) prior on theta
data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  // Beta(2, 2) prior contribution
  target += log(theta) + log(1 - theta);
  
  // likelihood contribution
  for (i in 1:N) {
    target += log(theta) * y[i] + log(1 - theta) * (1 - y[i]);
  }
}
