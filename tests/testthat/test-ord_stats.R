context("Ordinal Statistics")

testdata <- list(
  sample(1:5, 100, replace = TRUE),
  sample(1:5, 100, replace = TRUE)
)

test_that("ord_gamma returns single numeric value", {
  testobj <- ord_gamma(testdata[[1]], testdata[[2]])
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)
})

test_that("ord_somers_d returns single numeric value", {
  testobj <- ord_somers_d(testdata[[1]], testdata[[2]], reverse = FALSE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_somers_d(testdata[[1]], testdata[[2]], reverse = TRUE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_somers_d(testdata[[1]], testdata[[2]], symmetric = TRUE)
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)
})

test_that("ord_tau returns single numeric value", {
  testobj <- ord_tau(testdata[[1]], testdata[[2]], tau = "a")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_tau(testdata[[1]], testdata[[2]], tau = "b")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  testobj <- ord_tau(testdata[[1]], testdata[[2]], tau = "c")
  expect_is(testobj, "numeric")
  expect_length(testobj, 1)

  expect_error(ord_tau(testdata[[1]], testdata[[2]], tau = "KWBFKB"))
})

test_that("ord_pairs returns df of numeric values", {
  testobj <- ord_pairs(testdata[[1]], testdata[[2]])

  expect_is(testobj, "data.frame")
  expect_length(testobj, 5)
})

test_that("tadaa_ord yields dust object", {
  expect_is(tadaa_ord(testdata[[1]], testdata[[2]], print = "console"), "dust")
  expect_is(tadaa_ord(testdata[[1]], testdata[[2]], print = "markdown"), "dust")
  expect_is(tadaa_ord(testdata[[1]], testdata[[2]], print = "html"), "dust")
})
