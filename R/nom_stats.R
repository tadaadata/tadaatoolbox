#' Simple Chi^2
#'
#' This is a very simple wrapper for [stats::chisq.test].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param correct Apply correction, passed to `chisq.test`.
#' @return A `numeric` value
#' @export
#' @import stats
#' @note The warning message in case of low samples size and possibly incorrect
#' approximation is suppressed silently.
#' @examples
#' nom_chisqu(ngo$abschalt, ngo$geschl)
nom_chisqu <- function(x, y = NULL, correct = FALSE) {
  if (is.table(x)) {
    as.numeric(suppressWarnings(chisq.test(x = x, correct = correct)$statistic))
  } else {
    as.numeric(suppressWarnings(chisq.test(x = x, y = y, correct = correct)$statistic))
  }
}

#' Phi coefficient
#'
#' Very simple wrapper for [DescTools::Phi].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#'
#' @return `numeric` value
#' @export
#' @importFrom DescTools Phi
#' @examples
#' nom_phi(ngo$abschalt, ngo$geschl)
nom_phi <- function(x, y = NULL) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  DescTools::Phi(x)
}

#' Cramer's V
#'
#' Very simple wrapper for [DescTools::CramerV].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#'
#' @return `numeric` value
#' @export
#' @importFrom DescTools CramerV
#' @examples
#' nom_v(ngo$abschalt, ngo$geschl)
nom_v <- function(x, y = NULL) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  DescTools::CramerV(x)
}

#' Contingency Coefficient C
#'
#' Very simple wrapper for [DescTools::ContCoef].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#'
#' @return `numeric` value
#' @export
#' @importFrom DescTools ContCoef
#' @examples
#' nom_c(ngo$abschalt, ngo$geschl)
nom_c <- function(x, y = NULL) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  DescTools::ContCoef(x)
}

#' Lambda
#'
#' Very simple wrapper for [DescTools::Lambda].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param symmetric If `TRUE`, symmetric lambda is returned. Default is `FALSE`.
#' @param reverse If `TRUE`, row and column variable are switched.
#'
#' @return `numeric` value
#' @export
#' @importFrom DescTools Lambda
#' @examples
#' nom_lambda(ngo$abschalt, ngo$geschl)
nom_lambda <- function(x, y = NULL, symmetric = FALSE, reverse = FALSE) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  if (symmetric) {
    DescTools::Lambda(x, direction = "symmetric")
  } else if (!reverse) {
    DescTools::Lambda(x, direction = "row")
  } else {
    DescTools::Lambda(x, direction = "column")
  }
}

#' Get all the nominal stats
#'
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param round Ho many digits should be rounded. Default is 2.
#' @param print Print method. Passed to [pixiedust::sprinkle_print_method] as of now.
#' @return A `dust` object, depending on `print`.
#' @export
#' @import pixiedust
#' @family Tadaa-functions
#' @examples
#' tadaa_nom(ngo$abschalt, ngo$geschl)
tadaa_nom <- function(x, y = NULL, round = 2, print = "console") {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  chisq <- round(nom_chisqu(x), round)
  v <- round(nom_v(x), round)
  cc <- round(nom_c(x), round)
  lmbd_x <- round(nom_lambda(x), round)
  lmbd_y <- round(nom_lambda(x, reverse = TRUE), round)
  lmbd_s <- round(nom_lambda(x, symmetric = TRUE), round)

  ret <- data.frame(
    "chisq" = chisq, "cv" = v,
    "lmbd_x" = lmbd_x, "lmbd_y" = lmbd_y,
    "lmbd_s" = lmbd_s, "c" = cc
  )

  if (print == "markdown") {
    retprint <- pixiedust::sprinkle_colnames(
      pixiedust::dust(ret),
      chisq = "$\\chi^2$",
      cv = "Cramer's V",
      lmbd_x = "$\\lambda_x$",
      lmbd_y = "$\\lambda_y$",
      lmbd_s = "$\\lambda_{xy}$"
    )
  } else {
    retprint <- pixiedust::sprinkle_colnames(
      pixiedust::dust(ret),
      chisq = "Chi^2",
      cv = "Cramer's V",
      lmbd_x = "Lambda (x dep.)",
      lmbd_y = "Lambda (y dep.)",
      lmbd_s = "Lambda (sym.)"
    )
  }

  return(pixiedust::sprinkle_print_method(retprint, print))
}
