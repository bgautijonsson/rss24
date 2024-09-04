library(stdmatern)

X <- rmatern_copula_folded_full(n = 100, dim1 = 400, dim2 = 180, rho1 = 0.8, rho2 = 0.9, nu = 2)

apply(X, 1, mean) |> hist()
curve(3000 * dnorm(x, 0, 1 / 10), add = TRUE, from = -0.5, to = 0.5, col = "red")


apply(X, 1, var) |> hist()
curve(3.5e5 * dchisq(99 * x, df = 99), col = "red", from = 0, to = 2, add = TRUE)
