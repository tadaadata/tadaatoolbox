context("Two Sample Tests")


test_that("t-Test works", {
  expect_is(tadaa_t.test(ngo, deutsch, geschl, paired = FALSE, print = "df"), "data.frame")
  expect_is(tadaa_t.test(ngo, deutsch, geschl, paired = TRUE, print = "df"), "data.frame")

  expect_is(tadaa_t.test(ngo, deutsch, geschl, paired = TRUE, direction = "less", print = "df"), "data.frame")
  expect_is(tadaa_t.test(ngo, deutsch, geschl, paired = TRUE, direction = "greater", print = "df"), "data.frame")

  expect_is(tadaa_t.test(ngo, deutsch, geschl, print = "markdown"), "dust")
  expect_is(tadaa_t.test(ngo, deutsch, geschl, print = "html"), "dust")
  expect_is(tadaa_t.test(ngo, deutsch, geschl, print = "console"), "dust")
})

test_that("Wilcoxon test works", {
  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, paired = FALSE, print = "df"), "data.frame")
  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, paired = TRUE, print = "df"), "data.frame")

  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, paired = TRUE, direction = "less", print = "df"), "data.frame")
  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, paired = TRUE, direction = "greater", print = "df"), "data.frame")

  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, print = "markdown"), "dust")
  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, print = "html"), "dust")
  expect_is(tadaa_wilcoxon(ngo, deutsch, geschl, print = "console"), "dust")
})
