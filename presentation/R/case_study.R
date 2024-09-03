library(stdmatern)
library(evd)
library(tidyverse)
library(gt)

dim1 <- 50
dim2 <- 50
rho1 <- 0.9
rho2 <- 0.5
nu <- 1
n_obs <- 5
Z <- rmatern_copula_eigen(n_obs, dim1, dim2, rho1, rho2, nu)
U <- pnorm(Z)
Y <- qgev(U, loc = 6, scale = 2, shape = 0.1)

log_lik <- function(par, Y) {
  mu <- exp(par[1])
  sigma <- exp(par[2] + par[1])
  xi <- exp(par[3])
  rho1 <- plogis(par[4])
  rho2 <- plogis(par[5])

  ll_marg <- sum(dgev(Y, loc = mu, scale = sigma, shape = xi, log = TRUE))
  u <- pgev(Y, loc = mu, scale = sigma, shape = xi)
  z <- qnorm(u)
  ll_copula <- sum(dmatern_copula_eigen(z, dim1, dim2, rho1, rho2, nu))
  ll_copula + ll_marg
}


res <- optim(
  par = c(0, 0, 0, 0, 0),
  log_lik,
  control = list(maxit = 5000, fnscale = -1),
  Y = Y,
  hessian = TRUE,
  method = "L-BFGS-B"
)

se <- sqrt(diag(solve(-res$hessian)))

tibble(
  par = c("mu_", "sigma_", "xi_", "rho_1", "rho_2"),
  estimate = res$par,
  se = se
) |>
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) |>
  select(-se) |>
  pivot_longer(
    cols = c(estimate, lower, upper),
    names_to = "statistic",
    values_to = "value"
  ) |>
  pivot_wider(names_from = par, values_from = value) |>
  mutate(
    mu_ = exp(mu_),
    sigma_ = exp(sigma_) * mu_,
    xi_ = exp(xi_),
    rho_1 = plogis(rho_1),
    rho_2 = plogis(rho_2)
  ) |>
  pivot_longer(cols = -statistic, names_to = "par", values_to = "value") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  mutate(
    par = str_c("<b>&", par, "</sub></b>") |>
      str_replace("_", ";<sub>")
  ) |>
  gt() |>
  fmt_markdown(columns = par) |>
  fmt_number(decimals = 3) |>
  cols_label(
    par = "",
    estimate = "Estimate",
    lower = "Lower",
    upper = "Upper"
  ) |>
  tab_spanner(
    label = "95% CI",
    columns = c(lower, upper)
  )
