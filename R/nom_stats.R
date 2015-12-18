#' Get all the nominal stats
#'
#' @param x Dependent variable. Alternatively a \code{table}.
#' @param y Independent variable
#' @param print Print method. Passed to \link[pixiedust]{sprinkle_print_method} as of now.

#' @return A \code{dust} object, depending on \code{print}.
#' @export
#' @import qmtut
#' @import pixiedust
#' @examples
#' tadaa_nom(qmtut::ngo$abschalt, qmtut::ngo$geschl)
tadaa_nom <- function(x, y = NULL, round = 2, print = "console"){
  if (!is.table(x)) {
    x <- table(x, y)
  }
  chisq  <- round(qmtut::nom_chisqu(x), round)
  v      <- round(qmtut::nom_v(x), round)
  cc     <- round(qmtut::nom_c(x), round)
  lmbd_x <- round(qmtut::nom_lambda(x), round)
  lmbd_y <- round(qmtut::nom_lambda(x, reverse = T), round)
  lmbd_s <- round(qmtut::nom_lambda(x, symmetric = T), round)

  ret <- data.frame("chisq" = chisq, "cv" = v, "c" = cc,
                    "lmbd_x" = lmbd_x, "lmbd_y" = lmbd_y,
                    "lmbd_s" = lmbd_s)

  ret <- pixiedust::sprinkle_colnames(pixiedust::dust(ret), chisq = "Chi^2",
                                      cv = "Cramer's V",
                                      lmbd_x = "Lambda (x dependent)",
                                      lmbd_y = "Lambda (y dependent)",
                                      lmbd_s = "Lambda (symmetric)")
  pixiedust::sprinkle_print_method(ret, print)
}