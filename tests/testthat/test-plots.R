context("Tadaa, Plots!")

test_that("tadaa_int produces plot", {
  expect_is(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = FALSE, print = FALSE), "list")
  expect_is(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = FALSE, show_n = TRUE, print = FALSE)[[1]], "gg")
  expect_is(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = FALSE, print = FALSE)[[1]], "gg")
  expect_is(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = FALSE, print = FALSE)[[2]], "gg")
  expect_length(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = TRUE, print = FALSE), 2)

  if (requireNamespace("cowplot")) {
    expect_is(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = TRUE, print = FALSE)[[1]], "gg")
    expect_length(tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = FALSE, print = TRUE), 2)
  }
})

test_that("tadaa_balance produces plot", {
  expect_is(tadaa_balance(ngo, jahrgang, geschl), "gg")
  expect_is(tadaa_balance(ngo, jahrgang, geschl, annotate = TRUE), "gg")
})

test_that("tadaa_mean_ci procudes plot", {
  expect_is(tadaa_mean_ci(ngo, deutsch, jahrgang, brewer_palette = "Set1"), "gg")
})

test_that("tadaa_pairwise_tukey produces plot", {
  tests <- tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, geschl, print = "df")
  expect_is(tadaa_plot_tukey(tests), "gg")
})

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}

# theme_tadaa ----

test_that("Themes are themes", {
  expect_is(theme_tadaa(), c("theme", "gg"))
  expect_is(theme_tadaa(axis_emph = ""), c("theme", "gg"))
  expect_is(theme_tadaa(axis_emph = "x"), c("theme", "gg"))
  expect_is(theme_tadaa(axis_emph = "y"), c("theme", "gg"))
  expect_is(theme_tadaa(axis_emph = "xy"), c("theme", "gg"))
  expect_is(theme_readthedown(), c("theme", "gg"))

  expect_warning(theme_tadaa(axis_emph = "wurst"))
})
