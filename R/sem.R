#' Standard Error of the Mean
#'
#' @param x a numeric vector or R object which is coercible to one
#' @param conf.level the confidence level (alpha) of the Interval
#' @return a \code{data.frame} with the mean, SEM and it's Confidence Interval
#' @export
#' @examples
#' iq <- rnorm(100, 100, 15)
#'
#' tadaa_sem(iq)
tadaa_sem <- function(x, conf.level = .95) {
  # SEM if sigma is known // todo: add option or correct for length(x) and NAs
  sem  <- sd(x, na.rm = T) / sqrt(length(x))
  # clvl <- qnorm(1 - conf.level / 2)
  clvl <- qnorm(conf.level + ((1 - conf.level) / 2))

  CI_upper <- mean(x, na.rm = T) + (sem * clvl)
  CI_lower <- mean(x, na.rm = T) - (sem * clvl)

  bunch_of_stats <- data.frame(Mean  = mean(x, na.rm = T),
                               SEM   = sem,
                               lower = CI_lower,
                               upper = CI_upper)

 return(bunch_of_stats)
}
