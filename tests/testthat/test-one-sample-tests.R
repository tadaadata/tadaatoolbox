context("One Sample Tests")

test_that("One sample tests work works", {
  expect_is(tadaa_one_sample(ngo, deutsch, 8, print = "df"), "data.frame")
  expect_is(tadaa_one_sample(ngo, deutsch, 2, sigma = 3, print = "df"), "data.frame")

  expect_is(tadaa_one_sample(ngo, deutsch, 43, direction = "less", print = "df"), "data.frame")
  expect_is(tadaa_one_sample(ngo, deutsch, 21, direction = "greater", print = "df"), "data.frame")

  expect_is(tadaa_one_sample(ngo, deutsch, 30, print = "markdown"), "dust")
  expect_is(tadaa_one_sample(ngo, deutsch, 2, print = "html"), "dust")
  expect_is(tadaa_one_sample(ngo, deutsch, 293, print = "console"), "dust")
})
