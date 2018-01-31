#' Games Howell Post-Hoc Test
#'
#' An implementation of the Games Howell procedure for pairwise comparisons.
#' The workhorse of this function is adapted from this gist:
#' https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096
#' @inheritParams tadaa_pairwise_t
#' @return A `data.frame` or [pixiedust::dust] object depending on `print`.
#' @export
#' @references https://rpubs.com/aaronsc32/games-howell-test
#' @source https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096
#' @author `gitub.com/aschleg`, Lukas Burk
#' @import pixiedust
#' @family Tadaa-functions
#' @seealso [tadaa_pairwise_t()], [tadaa_pairwise_tukey()]
#' @note
#' This function is really, really slow for large comparisons (`k > 50`).
#' Sorry about that.
#' @examples
#' tadaa_pairwise_gh(ngo, deutsch, jahrgang)
#' tadaa_pairwise_gh(ngo, deutsch, jahrgang, geschl)
tadaa_pairwise_gh <- function(data, response, group1, group2 = NULL, print = "df") {
  response <- deparse(substitute(response))
  group1 <- deparse(substitute(group1))
  group2 <- deparse(substitute(group2))

  # Tests
  tests <- games_howell(data[[response]], data[[group1]])
  tests$term <- group1

  # If second group is defined, we do interactions and stuff
  if (group2 != "NULL") {
    data[["interaction"]] <- interaction(data[[group1]], data[[group2]], sep = " & ")

    tests_int <- games_howell(data[[response]], data[["interaction"]])
    tests_int$term <- paste0(group1, ":", group2)

    test_2 <- games_howell(data[[response]], data[[group2]])
    test_2$term <- group2

    tests <- rbind(tests, test_2, tests_int)
  }

  rownames(tests) <- NULL

  tests <- tests[c(
    "term", "comparison", "se", "t", "df",
    "conf_low", "conf_high", "adj.p.value"
  )]

  if (print == "df") {
    return(tests)
  } else {
    tests$term <- as.character(tests$term)
    tests$comparison <- as.character(tests$comparison)
    output <- pixiedust::dust(tests)
    output <- pixiedust::sprinkle(output, cols = "adj.p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle_colnames(output, adj.p.value = "p (adj.)")
    # output <- pixiedust::sprinkle_table(output, cols = 1, caption = "", part = "head")
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}

#' Games Howell post-hoc procedure
#' @param grp Grouping variable, i.e. `factor`.
#' @param obs Dependent, numeric vector.
#' @references https://rpubs.com/aaronsc32/games-howell-test
#' @source https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096
#' @author `gitub.com/aschleg`
#' @importFrom utils combn
#' @import stats
#' @keywords internal
games_howell <- function(obs, grp) {
  # Create combinations
  combs <- combn(unique(grp), 2)

  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)

  statistics <- lapply(1:ncol(combs), function(x) {
    mean.diff <- Mean[combs[2, x]] - Mean[combs[1, x]]

    # t-values
    t <- abs(Mean[combs[1, x]] - Mean[combs[2, x]]) /
      sqrt((std[combs[1, x]] / n[combs[1, x]]) +
        (std[combs[2, x]] / n[combs[2, x]]))

    # Degrees of Freedom
    df <- (std[combs[1, x]] / n[combs[1, x]] + std[combs[2, x]] / n[combs[2, x]]) ^ 2 / # num df
      ((std[combs[1, x]] / n[combs[1, x]]) ^ 2 / (n[combs[1, x]] - 1) + # Part 1 of denom df
        (std[combs[2, x]] / n[combs[2, x]]) ^ 2 / (n[combs[2, x]] - 1)) # Part 2 of denom df

    # p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)

    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1, x]] / n[combs[1, x]] + std[combs[2, x]] / n[combs[2, x]]))

    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]

    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]

    # Group Combinations
    grp.comb <- paste(combs[1, x], ":", combs[2, x])

    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, lower.conf, upper.conf)
  })

  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })

  # Create dataframe from flattened list
  results <- data.frame(matrix(
    unlist(stats.unlisted),
    nrow = length(stats.unlisted), byrow = TRUE
  ))

  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(
    as.numeric(as.matrix(
      results[c(2, 3:ncol(results))]
    )),
    digits = 3
  )

  # Rename data frame columns
  colnames(results) <- c(
    "comparison", "diff", "se", "t", "df",
    "adj.p.value", "conf_low", "conf_high"
  )

  results
}
