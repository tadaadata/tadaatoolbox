context("Ordinal Statistics")

testdata <- table(
  sample(1:5, 100, replace = TRUE),
  sample(1:5, 100, replace = TRUE)
)

test_that("ord_gamma returns single numeric value", {
  testobj <- ord_gamma(testdata)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)
})

test_that("ord_somers_d returns single numeric value", {
  testobj <- ord_somers_d(testdata, reverse = FALSE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_somers_d(testdata, reverse = TRUE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_somers_d(testdata, symmetric = TRUE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)
})

test_that("ord_tau returns single numeric value", {
  testobj <- ord_tau(testdata, tau = "a")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_tau(testdata, tau = "b")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_tau(testdata, tau = "c")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)
})

test_that("ord_pairs returns df of numeric values", {
  testobj <- ord_pairs(testdata)

  expect_is(testobj, "data.frame")
  expect_length(testobj, 5)
})
