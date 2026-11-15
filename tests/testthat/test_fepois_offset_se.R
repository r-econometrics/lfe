# Heteroskedasticity-robust (HC0) covariance of a Poisson glm, written out so
# the reference shares no code with fepois.
glm_hc0 <- function(fit) {
  X <- model.matrix(fit)
  mu <- fitted(fit)
  r <- fit$y - mu
  bread <- solve(crossprod(X, X * mu))
  meat <- crossprod(X * r)
  bread %*% meat %*% bread
}

fepois_data <- function(seed = 1, n = 500) {
  set.seed(seed)
  d <- data.frame(g = factor(sample(1:10, n, TRUE)), x1 = rnorm(n), x2 = rnorm(n))
  d$y <- rpois(n, exp(0.3 * d$x1 - 0.2 * d$x2 + as.numeric(d$g) / 10))
  d
}

test_that("fepois standard errors with an offset match the HC0 sandwich", {
  d <- fepois_data()
  # an offset correlated with a regressor, as log exposure often is
  d$lexpo <- log(runif(nrow(d), 0.5, 5)) + 0.5 * d$x1
  d$y <- rpois(nrow(d), exp(d$lexpo + 0.3 * d$x1 - 0.2 * d$x2 + as.numeric(d$g) / 10))
  fit <- fepois(y ~ x1 + x2 | g, d, offset = ~lexpo)
  ref <- glm(y ~ x1 + x2 + g + offset(lexpo), poisson, d)
  expect_equal(unname(drop(fit$coefficients)), unname(coef(ref)[c("x1", "x2")]), tolerance = 1e-6)
  expect_equal(unname(drop(fit$se)), unname(sqrt(diag(glm_hc0(ref)))[c("x1", "x2")]), tolerance = 1e-6)
})
