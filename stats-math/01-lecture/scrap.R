print_stack <- function() {
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    call <- paste(deparse(calls[[i]]), collapse = " ")
    cat(sprintf("%2d: %s\n", i, call))
  }
}

fact <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * fact(n - 1))
  }
}

fact_trace <- function(n) {
  cat("Call: fact(", n, ")\n", sep = "")

  if (n == 0) {
    cat("Base case: fact(0) = 1\n")
    return(1)
  }

  previous <- fact_trace(n - 1)
  result <- n * previous

  cat(
    "Return: fact(", n, ") = ",
    n, " × ", previous, " = ", result,
    "\n",
    sep = ""
  )
  return(result)
}

fib <- function(n) {
  if (n == 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  } else {
    return(fib(n - 1) + fib(n - 2))
  }
}

fib_seq <- function(n) {
  s <- integer(n)
  s[1] <- 1L; s[2] <- 1L
  for (i in 3:n) {
    s[i] <- s[i - 1] + s[i - 2]
  }
  return(s)
}

fib_seq(10)

library(purrr)
N <- 20 # N must be even
fibN <- fib_seq(N)
odd_fib <- fibN[seq_len(N) %% 2 != 0]
sum(odd_fib) == fib(N)

N <- 15 # N must be odd
fibN <- fib_seq(N)
even_fib <- fibN[seq_len(N) %% 2 == 0]
sum(even_fib) == fib(N) - 1


gcd <- function(a, b) {
  if (b == 0) {
    return(abs(a))
  } else {
    return(gcd(b, a %% b))
  }
}

bin_counter <- function(n_bits = 4) {
  max_power_of_two <- n_bits - 1
  place_values <- 2^(max_power_of_two:0)
  max_num <- 2^n_bits - 1

  for (number in 0:max_num) {
    bits <- (number %/% place_values) %% 2
    binary_number <- paste(bits, collapse = "")
    cat(binary_number, "\n")
  }
}
bin_counter()

N <- 100
N^2 * (N + 1)/2  - sum(1:(N - 1) * (N - 1):1)
sum((1:N)^2)

library(HistData); library(ggplot2); library(dplyr)
data("Virginis.interp")

Virginis.interp |>
  mutate(x = distance * sin(posangle * pi/180),   # East
         y = distance * cos(posangle * pi/180)) |># North
  ggplot(aes(x, y)) +
  geom_path() + geom_point() +
  annotate("point", x = 0, y = 0, size = 4) +
  scale_x_reverse() +                              # East to the left
  coord_fixed() +
  labs(x = "East  →  (arb. units)", y = "North")

# Herschel's interpolated curve
plot(posangle ~ year, data = Virginis.interp,
     pch = 15, type = "b", col = "red", cex = 0.8, lwd = 2)

# The data points, and indication of their uncertainty
points(posangle ~ year, data = Virginis, pch = 16)
points(posangle ~ year, data = Virginis, cex = weight / 2)     # circle size = weight

m <- matrix(sample(12), ncol = 3)

a <- 10
s <- 0
for (i in 1:4) {
  for (j in 1:3) {
    s <- s + m[i, j]
    #    cat("s =", s, "i =", i, "j=", j, "\n")
  }
}

sum_mat <- function(x, a = 1, print = FALSE) {
  I <- nrow(x)
  J <- ncol(x)
  s <- 0 # see what happens if you forget to set it here
  for (i in 1:I) {
    for (j in 1:J) {
      s <- s + a * x[i, j]
      if (print) {
        cat("s =", s, "i =", i, "j=", j, "\n")
      }
    }
  }
  return(s)
}

softmax_unsafe <- function(x) exp(x) / sum(exp(x))
y_small <- 1:3
softmax_unsafe(y_small)

y_large <- c(1e3, 1e3 + 1, 1e3 + 2)
exp(y_large)
softmax_unsafe(y_large)

# you can compare integers this way, but not real numbers. try it.
100 * sum_mat(m, a = 1) == sum_mat(m, a = 100) 

# lots of ways to do the same thing in R
sum(m)


rowSums(m)
colSums(m)

apply(m, 1, prod) # same as rowSums(x)


apply(m, 2, sum) # same as colSums(x)
apply(m, 1, sum) |> sum()
apply(m, 2, sum) |> sum()

set.seed(2006)
mv <- numeric(10)
for (i in 1:10) {
  x <- runif(100, min = i, max = 15)
  mv[i] <- mean(x)
}

# from Lecture 4 homework
n <- 1e5
die <- 1:6
denom <- 0
numer <- 0
for (i in 1:n) {
  s <- sample(die, 2, replace = TRUE)
  if (sum(s) == 8) {
    denom <- denom + 1
    if (s[1] == 5 || s[2] == 5)
      numer <- numer + 1
  }
}
numer / denom

library(dplyr)
iris |>
  group_by(Species) |>
  summarise(
    across(where(is.numeric), mean),
    .groups = "drop"
  )

set.seed(502)
geometric_theta <- 0.2
geometric_reps <- 50000
first_success <- rgeom(geometric_reps, prob = geometric_theta) + 1
estimate_empirical_probability <- function(value, simulated_values) {
  number_of_matches <- sum(simulated_values == value)
  number_of_simulations <- length(simulated_values)
  number_of_matches / number_of_simulations
}


geometric_comparison <- data.frame(t = 1:10) |>
  mutate(
    t,
    empirical = purrr::map_dbl(
      estimate_empirical_probability,
      simulated_values = first_success
    ),
    theoretical = geometric_theta * (1 - geometric_theta)^(t - 1),
    difference = empirical - theoretical
  )

set.seed(503)
rare_reps <- 100000
rare_size <- 1000
rare_prob <- 0.003
poisson_lambda <- rare_size * rare_prob
rare_counts <- rbinom(
  rare_reps,
  size = rare_size,
  prob = rare_prob
)
# Reuse the empirical-probability function from Problem 2
rare_pmf <- data.frame(k = 0:10) |>
  dplyr::mutate(
    empirical_binomial = purrr::map_dbl(
      k,
      estimate_empirical_probability,
      simulated_values = rare_counts
    ),
    exact_binomial = dbinom(k, size = rare_size, prob = rare_prob),
    poisson = dpois(k, lambda = poisson_lambda)
  )



