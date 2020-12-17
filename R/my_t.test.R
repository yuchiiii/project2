#' T test function
#'
#' This function performs a one sample t-test.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#' @param mu A number indicating the null hypothesis value of the mean.
#' @keywords inference
#' @return A list with elements the numeric test statistic \code{test stat},
#'   the degrees of freedom \code{df}, the value of the parameter \code{alternative},
#'   and the numeric p-value \code{p_val}.
#' @examples
#' my_t.test(1:10, "two.sided", 0)
#' my_t.test(1:10, "less", 1)
#' @export
my_t.test <- function(x, alternative, mu) {
  # Check if there is an error in the parameter.
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("Wrong alternative hypothesis!")
  }

  # Calculate the values.
  x_mean <- mean(x)
  size <- length(x)
  df = size - 1
  sd_error <- sd(x) / sqrt(size)
  t_obs <- (x_mean - mu) / sd_error

  # Find the p-value according to the alternative hypothesis.
  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
  } else if (alternative == "less" && t_obs > 0) {
    p_value <- 1 - pt(t_obs, df, lower.tail = FALSE)
  } else if (alternative == "greater" && t_obs < 0) {
    p_value <- 1 - pt(abs(t_obs), df, lower.tail = FALSE)
  } else {
    p_value <- pt(abs(t_obs), df, lower.tail = FALSE)
  }

  # Create a list that saves the results.
  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)
  return(result)
}
