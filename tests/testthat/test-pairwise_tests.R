context("Pairwise Tests")

# pairwise t-test ----

test_that("Pairwise t-Tests results are correct data structure", {
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "none", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "bonf", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidak", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidakSD", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "holm", print = "console"), "dust")

  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "none", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "bonf", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidak", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidakSD", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "holm", print = "markdown"), "dust")

  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "none", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "bonf", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidak", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "sidakSD", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, p.adjust = "holm", print = "df"), "data.frame")
})

test_that("Pairwise t-Tests with interaction results are correct data structure", {
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "none", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "bonf", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidak", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidakSD", print = "console"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "holm", print = "console"), "dust")

  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "none", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "bonf", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidak", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidakSD", print = "markdown"), "dust")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "holm", print = "markdown"), "dust")

  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "none", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "bonf", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidak", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidakSD", print = "df"), "data.frame")
  expect_is(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "holm", print = "df"), "data.frame")

  expect_error(tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "qwfhqf", print = "df"))
})

# pairwise tukey ----

test_that("Pairwise tukey returns correct data structure", {
  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, print = "console"), "dust")
  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, print = "html"), "dust")
  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, print = "df"), "data.frame")

  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, geschl, print = "console"), "dust")
  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, geschl, print = "html"), "dust")
  expect_is(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, geschl, print = "df"), "data.frame")

  expect_error(tadaa_pairwise_tukey(ngo, deutsch, jahrgang, geschl, print = "wurst"))
})

# games howell ----

test_that("Games Howell returns correct data structure", {
  skip_on_cran()
  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, print = "console"), "dust")
  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, print = "html"), "dust")
  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, print = "df"), "data.frame")

  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, geschl, print = "console"), "dust")
  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, geschl, print = "html"), "dust")
  expect_is(tadaa_pairwise_gh(ngo, deutsch, jahrgang, geschl, print = "df"), "data.frame")

  expect_error(tadaa_pairwise_gh(ngo, deutsch, jahrgang, geschl, print = "wurst"))
})
