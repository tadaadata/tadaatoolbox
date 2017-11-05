#' Gamma
#'
#' Simple wrapper for \link[ryouready]{ord.gamma}.
#'
#' @param x A \code{table} or dependent numeric variable.
#' @param y Empty or independent grouping variable
#'
#' @return \code{numeric} of length 1.
#' @export
#' @importFrom ryouready ord.gamma
#' @examples
#' df <- data.frame(rating = round(runif(50, 1, 5)),
#'                  group  = sample(c("A", "B", "C"), 50, TRUE))
#' tbl <- table(df)
#' ord_gamma(tbl)
ord_gamma <- function(x, y = NULL){
  if (!is.table(x)) {
    x <- table(x, y)
  }
  gamma <- as.numeric(ryouready::ord.gamma(x))
  return(gamma)
}

#' Somers' D
#'
#' Very simple wrapper for \link[ryouready]{ord.somers.d}.
#' @param x Dependent variable. Alternatively a \code{table}.
#' @param y Independent variable
#' @param symmetric If \code{TRUE}, symmetric D is returned. Default is \code{FALSE}.
#' @param reverse If \code{TRUE}, row and column variable are switched.
#' @return \code{numeric} value
#' @export
#' @importFrom ryouready ord.somers.d
#' @examples
#' ord_somers_d(ngo$abschalt, ngo$geschl)
ord_somers_d <- function(x, y = NULL, symmetric = FALSE, reverse = FALSE){
  if (!is.table(x)) {
    x <- table(x, y)
  }
  if (symmetric) {
    ryouready::ord.somers.d(x)$sd.symmetric
  } else if (!reverse) {
    ryouready::ord.somers.d(x)$sd.rc
  } else {
    ryouready::ord.somers.d(x)$sd.cr
  }
}

#' Various Tau Statistics
#'
#' A wrapper for the appropriate functions form \link{DescTools} to calculate
#' Tau A, B and C.
#' @param x Dependent variable. Alternatively a \code{table}.
#' @param y Independent variable
#' @param tau Which of the Taus to return. Default is \code{"b"}.
#' @param reverse If \code{TRUE}, row and column variable are switched.
#' @return \code{numeric} value
#' @export
#' @importFrom DescTools KendallTauA
#' @importFrom DescTools StuartTauC
#' @importFrom DescTools KendallTauB
#' @examples
#' ord_tau(ngo$urteil, ngo$leistung, tau = "a")
ord_tau <- function(x, y = NULL, tau = "b", reverse = FALSE) {
  if (!is.table(x)) {
    x <- table(x, y)
  }

  direction <- ifelse(reverse, "column", "row")

  if (tau %in% c("a", "A")) {
    ret <- DescTools::KendallTauA(x, direction = direction)
  } else if (tau %in% c("b", "B")) {
    ret <- DescTools::KendallTauB(x, direction = direction)
  } else if (tau %in% c("c", "C")) {
    ret <- DescTools::StuartTauC(x, direction = direction)
  } else {
    stop("You need to specifiy which tau to return")
  }

  ret
}

#' Get all the ordinal stats
#'
#' As of now, only Gamma and Somers D are supported. But let's be honest: Everybody hates Tau.
#' @param x Dependent variable. Alternatively a \code{table}.
#' @param y Independent variable
#' @param round Ho many digits should be rounded. Default is 2.
#' @param print Print method. Passed to \link[pixiedust]{sprinkle_print_method} as of now.
#' @return A \code{dust} object, depending on \code{print}.
#' @export
#' @import pixiedust
#' @family Tadaa-functions
#' @examples
#' tadaa_ord(ngo$abschalt, ngo$geschl)
tadaa_ord <- function(x, y = NULL, round = 2, print = "console"){
  if (!is.table(x)) {
    x <- table(x, y)
  }

  gamma  <- round(ord_gamma(x), round)

  somer_x <- round(ord_somers_d(x), round)
  #somer_y <- round(ord_somers_d(x, reverse = T), round)
  somer_s <- round(ord_somers_d(x, symmetric = T), round)

  tau_a <- round(ord_tau(x, tau = "a"), round)
  tau_b <- round(ord_tau(x, tau = "b"), round)
  tau_c <- round(ord_tau(x, tau = "c"), round)

  ret <- data.frame("gamma" = gamma, "somer_x" = somer_x, "somer_s" = somer_s,
                    "tau_a" = tau_a, "tau_b" = tau_b, "tau_c" = tau_c)

  if (print != "markdown") {
    retprint <- pixiedust::sprinkle_colnames(pixiedust::dust(ret),
                                             gamma = "Gamma",
                                             somer_x = "Somers' D",
                                             somer_s = "Somers' D (sym.)",
                                             tau_a = "Tau A",
                                             tau_b = "Tau B",
                                             tau_c = "Tau C")
  } else {
    retprint <- pixiedust::sprinkle_colnames(pixiedust::dust(ret),
                                             gamma = "$\\gamma$",
                                             somer_x = "Somers' D",
                                             somer_s = "Somers' D (sym.)",
                                             tau_a = "$\\tau_A$",
                                             tau_b = "$\\tau_B$",
                                             tau_c = "$\\tau_C$")
  }

  return(pixiedust::sprinkle_print_method(retprint, print))
}
