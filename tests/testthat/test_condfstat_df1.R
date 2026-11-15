test_that("condfstat with one endogenous variable tests only the excluded instruments", {
  set.seed(11)
  n <- 800
  d <- data.frame(g = factor(sample(1:60, n, TRUE)), z1 = rnorm(n), z2 = rnorm(n), w1 = rnorm(n), w2 = rnorm(n))
  u <- rnorm(n)
  d$x <- 0.15 * d$z1 + 0.1 * d$z2 + 0.3 * d$w1 + 0.5 * u + rnorm(n)
  d$y <- 0.5 * d$x + 0.2 * d$w1 - 0.1 * d$w2 + u
  est <- felm(y ~ w1 + w2 | g | (x ~ z1 + z2), d)
  cf <- condfstat(est, type = "iid")
  # first-stage F for the two excluded instruments, from lm
  ref <- anova(lm(x ~ w1 + w2 + g, d), lm(x ~ w1 + w2 + g + z1 + z2, d))
  expect_equal(attr(cf, "df1"), 2)
  expect_equal(as.numeric(cf), ref$F[2], tolerance = 1e-6)
})
