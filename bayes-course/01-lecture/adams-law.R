library(purrr)
n <- 20
class_size <- sample(n)
mean_vec <- runif(n = n, 2.8, 3.2)
N <- sum(class_size)
W <- class_size / N
G <- class_size |>
  map(\(x) rnorm(n = x, mean = mean_vec, sd = 1))

# E(Y)
EY <- unlist(G) |> mean()
print(EY)
# E(E(Y | X))
gpaXprob <- map_dbl(G, mean) * W
EEYX <- sum(gpaXprob)
print(EEYX)