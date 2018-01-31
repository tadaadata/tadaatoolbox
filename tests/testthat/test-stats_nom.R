context("Nominal Statistics")

test_that("nom_chisq returns single numeric value greater 0", {
  ret <- nom_chisqu(sample(letters, 10), sample(letters, 10))
  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)

  ret <- nom_chisqu(table(
    sample(letters[1:4], 10, TRUE),
    sample(letters[1:4], 10, TRUE)
  ))

  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)
})

test_that("nom_v returns single numeric value greater 0", {
  ret <- nom_v(sample(letters, 10), sample(letters, 10))
  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)

  ret <- nom_v(table(
    sample(letters[1:4], 10, TRUE),
    sample(letters[1:4], 10, TRUE)
  ))

  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)
})

test_that("nom_phi returns single numeric value greater 0", {
  ret <- nom_phi(sample(letters, 10), sample(letters, 10))
  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)

  ret <- nom_phi(table(
    sample(letters[1:4], 10, TRUE),
    sample(letters[1:4], 10, TRUE)
  ))

  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)
})

test_that("nom_lambda returns single numeric value greater 0", {
  ret <- nom_lambda(sample(letters, 10), sample(letters, 10))
  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)

  ret <- nom_lambda(table(
    sample(letters[1:4], 10, TRUE),
    sample(letters[1:4], 10, TRUE)
  ))

  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)
})

test_that("nom_c returns single numeric value greater 0", {
  ret <- nom_c(sample(letters, 10), sample(letters, 10))
  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)

  ret <- nom_c(table(
    sample(letters[1:4], 10, TRUE),
    sample(letters[1:4], 10, TRUE)
  ))

  expect_is(ret, class = "numeric")
  expect_gte(ret, expected = 0)
})

# tadaa_nom ----

test_that("tadaa_nom returns dust object", {
  expect_is(tadaa_nom(sample(letters, 10), sample(letters, "10"), print = "markdown"), "dust")
  expect_is(tadaa_nom(sample(letters, 10), sample(letters, "10"), print = "console"), "dust")
  expect_is(tadaa_nom(sample(letters, 10), sample(letters, "10"), print = "html"), "dust")
})

# tadaa_chisq ----

test_that("tadaa_chisq works", {
  expect_is(tadaa_chisq(ngo, abschalt, jahrgang), "data.frame")
  expect_is(tadaa_chisq(ngo, abschalt, jahrgang, correct = FALSE), "data.frame")
  expect_is(tadaa_chisq(ngo, abschalt, jahrgang, correct = TRUE), "data.frame")
  expect_is(tadaa_chisq(ngo, abschalt, jahrgang, print = "markdown"), "dust")
})
