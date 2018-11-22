context("invert scales")

test_that("inverting is right", {
  expect_equal(inv(1:5, 1, 5), c(5:1))
  expect_equal(inv(-3:3, -3, 3), 3:-3)
})
