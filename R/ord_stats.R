#' Gamma
#'
#' Simple wrapper for [DescTools::GoodmanKruskalGamma].
#'
#' @param x A `table` or dependent numeric variable.
#' @param y Empty or independent grouping variable
#'
#' @return `numeric` of length 1.
#' @export
#' @importFrom DescTools GoodmanKruskalGamma
#' @examples
#' df <- data.frame(rating = round(runif(50, 1, 5)),
#'                  group  = sample(c("A", "B", "C"), 50, TRUE))
#' tbl <- table(df)
#' ord_gamma(tbl)
ord_gamma <- function(x, y = NULL) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  gamma <- DescTools::GoodmanKruskalGamma(x)
  return(gamma)
}

#' Somers' D
#'
#' Very simple wrapper for [DescTools::SomersDelta].
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param symmetric If `TRUE`, symmetric D is returned. Default is `FALSE`.
#' @param reverse If `TRUE`, row and column variable are switched. Default is `FALSE`,
#' meaning the row variable is considered dependant.
#' @return `numeric` value
#' @export
#' @importFrom DescTools SomersDelta
#' @examples
#' ord_somers_d(ngo$abschalt, ngo$geschl)
ord_somers_d <- function(x, y = NULL, symmetric = FALSE, reverse = FALSE) {
  if (!is.table(x)) {
    x <- table(x, y)
  }
  if (symmetric) {
    mean(c(
      DescTools::SomersDelta(x, direction = "row"),
      DescTools::SomersDelta(x, direction = "column")
    ))
  } else if (!reverse) {
    DescTools::SomersDelta(x, direction = "row")
  } else {
    DescTools::SomersDelta(x, direction = "column")
  }
}

#' Various Tau Statistics
#'
#' A wrapper for the appropriate functions from [DescTools] to calculate
#' Tau A, B and C.
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param tau Which of the Taus to return. Default is `"b"`.
#' @param reverse If `TRUE`, row and column variable are switched.
#' @return `numeric` value
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
#' Collects all `ord_` statistics in neat output.
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @param round Ho many digits should be rounded. Default is 2.
#' @param print Print method. Passed to [pixiedust::sprinkle_print_method] as of now.
#' @return A `dust` object, depending on `print`.
#' @export
#' @import pixiedust
#' @family Tadaa-functions
#' @examples
#' tadaa_ord(ngo$leistung, ngo$begabung)
tadaa_ord <- function(x, y = NULL, round = 2, print = "console") {
  if (!is.table(x)) {
    x <- table(x, y)
  }

  gamma <- round(ord_gamma(x), round)

  somer_x <- round(ord_somers_d(x), round)
  somer_y <- round(ord_somers_d(x, reverse = TRUE), round)
  somer_s <- round(ord_somers_d(x, symmetric = TRUE), round)

  tau_a <- round(ord_tau(x, tau = "a"), round)
  tau_b <- round(ord_tau(x, tau = "b"), round)
  tau_c <- round(ord_tau(x, tau = "c"), round)

  ret <- data.frame(
    "gamma" = gamma,
    "somer_x" = somer_x, "somer_y" = somer_y, "somer_s" = somer_s,
    "tau_a" = tau_a, "tau_b" = tau_b, "tau_c" = tau_c
  )

  if (print != "markdown") {
    retprint <- pixiedust::sprinkle_colnames(
      pixiedust::dust(ret),
      gamma = "Gamma",
      somer_x = "D (x)",
      somer_y = "D (y)",
      somer_s = "D (xy)",
      tau_a = "Tau A",
      tau_b = "Tau B",
      tau_c = "Tau C"
    )
  } else {
    retprint <- pixiedust::sprinkle_colnames(
      pixiedust::dust(ret),
      gamma = "$\\gamma$",
      somer_x = "$D_x$",
      somer_y = "$D_y$",
      somer_s = "$D_{xy}$",
      tau_a = "$\\tau_A$",
      tau_b = "$\\tau_B$",
      tau_c = "$\\tau_C$"
    )
  }

  return(pixiedust::sprinkle_print_method(retprint, print))
}

#' Retrieve all type of pairs for ordinal statistics
#'
#'
#' @param x Dependent variable. Alternatively a `table`.
#' @param y Independent variable
#' @return A 1x5 `data.frame` with numeric values.
#' @source Internals for this function are copied from
#' [this gist](https://gist.github.com/marcschwartz/3665743) by GitHub user Marc Schwartz.
#' @export
#'
#' @examples
#' ord_pairs(ngo$leistung, ngo$begabung)
ord_pairs <- function(x, y = NULL) {
  if (!is.table(x)) {
    x <- table(x, y)
  }

  data.frame(
    nc = concordant(x),
    nd = discordant(x),
    tr = ties.row(x),
    tc = ties.col(x),
    total = (sum(x) * (sum(x) - 1)) / 2
  )
}


# Calculate Pairs tied on Rows
# cycle through each row of x and multiply by
# sum(x elements to the right of x[r, c])
# x = table
#' @keywords internal
ties.row <- function(x) {
  x <- matrix(as.numeric(x), dim(x))

  total.pairs <- 0

  rows <- dim(x)[1]
  cols <- dim(x)[2]

  for (r in 1:rows)
  {
    for (c in 1:(cols - 1))
    {
      total.pairs <- total.pairs + (x[r, c] * sum(x[r, (c + 1):cols]))
    }
  }

  total.pairs
}

# Calculate Pairs tied on Columns
# cycle through each col of x and multiply by
# sum(x elements below x[r, c])
# x = table
#' @keywords internal
ties.col <- function(x) {
  x <- matrix(as.numeric(x), dim(x))

  total.pairs <- 0

  rows <- dim(x)[1]
  cols <- dim(x)[2]

  for (c in 1:cols)
  {
    for (r in 1:(rows - 1))
    {
      total.pairs <- total.pairs + (x[r, c] * sum(x[(r + 1):rows, c]))
    }
  }

  total.pairs
}

# Calculate CONcordant Pairs in a table
# cycle through x[r, c] and multiply by
# sum(x elements below and to the right of x[r, c])
# x = table
#' @keywords internal
concordant <- function(x) {
  x <- matrix(as.numeric(x), dim(x))

  # get sum(matrix values > r AND > c)
  # for each matrix[r, c]
  mat.lr <- function(r, c) {
    lr <- x[(r.x > r) & (c.x > c)]
    sum(lr)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.lr, r = r.x, c = c.x))
}

# Calculate DIScordant Pairs in a table
# cycle through x[r, c] and multiply by
# sum(x elements below and to the left of x[r, c])
# x = table
#' @keywords internal
discordant <- function(x) {
  x <- matrix(as.numeric(x), dim(x))

  # get sum(matrix values > r AND < c)
  # for each matrix[r, c]
  mat.ll <- function(r, c) {
    ll <- x[(r.x > r) & (c.x < c)]
    sum(ll)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.ll, r = r.x, c = c.x))
}
