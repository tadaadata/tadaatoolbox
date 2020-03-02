context("Tadaa, Assumptions!")


test_that("tadaa_levene works", {
  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, print = "console"), "dust")
  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, print = "markdown"), "dust")
  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, print = "df"), "data.frame")

  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, center = "mean", print = "console"), "dust")
  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, center = "mean", print = "markdown"), "dust")
  expect_is(tadaa_levene(ngo, deutsch ~ jahrgang * geschl, center = "mean", print = "df"), "data.frame")
})
