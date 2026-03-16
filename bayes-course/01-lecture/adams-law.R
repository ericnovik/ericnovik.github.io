library(purrr)

n <- 20
class_sizes <- sample(n)
mean_gpa_vec <- runif(n = n, 2.8, 3.2)
N <- sum(class_sizes)
W <- class_sizes / N
G <- class_sizes |>
  map(\(x) rnorm(n = x, mean = mean_gpa_vec, sd = 1))

# E(Y)
EY <- unlist(G) |> mean()
print(EY)

# E(E(Y | X))
class_means <- map_dbl(G, mean) # can't just average those
gpaXprob <- class_means * W
EEYX <- sum(gpaXprob)
print(EEYX)
