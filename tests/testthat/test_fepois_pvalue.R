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

test_that("fepois p-values are two-sided", {
  d <- fepois_data()
  # small effects keep the p-values away from zero, where all.equal would
  # switch to an absolute comparison and miss a factor of two
  set.seed(3)
  d$y <- rpois(nrow(d), exp(0.05 * d$x1 + as.numeric(d$g) / 10))
  fit <- fepois(y ~ x1 + x2 | g, d)
  ref <- glm(y ~ x1 + x2 + g, poisson, d)
  se <- sqrt(diag(glm_hc0(ref)))[c("x1", "x2")]
  p_ref <- 2 * pnorm(-abs(coef(ref)[c("x1", "x2")] / se))
  expect_true(all(p_ref > 0.01))
  expect_equal(unname(drop(fit$pval) / p_ref), c(1, 1), tolerance = 1e-6)
  expect_equal(unname(drop(fit$rpval) / p_ref), c(1, 1), tolerance = 1e-6)
})

test_that("fepois p-values do not underflow for large statistics", {
  set.seed(2)
  n <- 2000
  d <- data.frame(g = factor(sample(1:10, n, TRUE)), x = rnorm(n))
  d$y <- rpois(n, exp(0.5 * d$x))
  fit <- fepois(y ~ x | g, d)
  expect_gt(abs(drop(fit$tval)), 9)
  expect_gt(drop(fit$pval), 0)
})
