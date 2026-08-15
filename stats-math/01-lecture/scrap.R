x <- matrix(sample(12), ncol = 3)
x

a <- 10
s <- 0
for (i in 1:4) {
  for (j in 1:3) {
    s <- s + x[i, j]
    cat("s =", s, "\n")
  }
}

# lots of ways to do the same thing in R
sum(x)
rowSums(x) |> sum() # |> is a shortcut for sum(rowSums(x))
colSums(x) |> sum()
apply(x, 1, sum) # same as rowSums(x)
apply(x, 2, sum) # same as colSums(x)
apply(x, 1, sum) |> sum()
apply(x, 2, sum) |> sum()


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
