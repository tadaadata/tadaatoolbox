context("CI Helpers")

df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))

test_that("confint_t returns one numerical value", {
  expect_is(confint_t(df$x), "numeric")
  expect_length(confint_t(df$x), 1)
})

test_that("confint_norm returns one numerical value", {
  expect_is(confint_norm(df$x), "numeric")
  expect_length(confint_norm(df$x), 1)
})

test_that("mean_ci_sem returns one numerical value", {
  expect_is(mean_ci_sem(df$x), "data.frame")
  expect_length(mean_ci_sem(df$x), 4)
})

test_that("effect_size returns one numerical value", {
  expect_true(is.numeric(effect_size_t(df, "x", "y")))
  expect_true(length(effect_size_t(df, "x", "y")) == 1)
})
