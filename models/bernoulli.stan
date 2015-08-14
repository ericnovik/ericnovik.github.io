data {
  int<lower=1> N; # check that we have at least one observation
  int<lower=0, upper=1> y[N]; # outcome y must be either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  real log_theta;
  real log1m_theta;
  log_theta <- log(theta);
  log1m_theta <- log1m(theta);
  for (n in 1:N)
    if (y[n] == 1)
      increment_log_prob(log_theta);
   else
     increment_log_prob(log1m_theta);
}

