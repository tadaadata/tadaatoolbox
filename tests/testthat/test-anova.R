context("Tadaa, ANOVA!")

test_that("tadaa_aov returns correct data structure", {
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 1, print = "console"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 2, print = "console"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 3, print = "console"), "dust")

  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 1, print = "html"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 2, print = "html"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 3, print = "html"), "dust")

  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 1, print = "df"), "data.frame")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 2, print = "df"), "data.frame")
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 3, print = "df"), "data.frame")

  expect_is(tadaa_aov(stunzahl ~ jahrgang, data = ngo, type = 1, print = "markdown"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang, data = ngo, type = 2, print = "markdown"), "dust")
  expect_is(tadaa_aov(stunzahl ~ jahrgang, data = ngo, type = 3, print = "markdown"), "dust")

  ngo_2 <- ngo
  contrasts(ngo_2$jahrgang) <- contr.treatment(levels(ngo$jahrgang))
  expect_is(tadaa_aov(stunzahl ~ jahrgang * geschl, type = 3, data = ngo_2), "data.frame")

  ngo_2$jahrgang <- as.character(ngo$jahrgang)
  expect_warning(tadaa_aov(stunzahl ~ jahrgang, data = ngo_2))
})

# Kruskal Wallis ----

test_that("Kruskal Wallis works", {
  expect_is(tadaa_kruskal(stunzahl ~ jahrgang, data = ngo, print = "df"), "data.frame")
  expect_is(tadaa_kruskal(stunzahl ~ jahrgang, data = ngo, print = "markdown"), "dust")
  expect_is(tadaa_kruskal(stunzahl ~ jahrgang, data = ngo, print = "console"), "dust")

  expect_error(tadaa_kruskal(stunzahl ~ jahrgang, data = ngo, print = "wurst"))
})
