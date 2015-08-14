data {
  int<lower=1> N; # check the we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real theta; 
}
model {
  theta ~ normal(0.5, 0.2); # prior on theta: 95% of the mass is between 0.1 and 0.9

  increment_log_prob(0); # this loop builds up the log posterior one observation ata a time.
  for (n in 1:N)
    increment_log_prob(y[n] * log(theta) + (1 - y[n]) * log(1 - theta));

  # the above three lines are equivalent to the following sampling statement:
  # y ~ bernoulli(theta);  
}
